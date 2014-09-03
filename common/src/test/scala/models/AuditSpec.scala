package test.models

import java.util.Date
import org.scalatest.{ FunSpec, Matchers }
import org.buttercoin.common.models.audit._
import org.buttercoin.common.util.Stateful

class AuditSpec extends FunSpec with Matchers {

  sealed trait TestEvent
  case object CreateTestEvent extends TestEvent {
    override def toString: String = "Created"
  }
  case object ModifyTestEvent extends TestEvent {
    override def toString: String = "Modified"
  }
  case object DeleteTestEvent extends TestEvent {
    override def toString: String = "Deleted"
  }

  case class TestHistory(info: String)

  case class TestAuditHistory(subject: TestHistory, events: List[AuditEvent[TestEvent]]) extends Audit[TestHistory, TestEvent] with Stateful {
    type This = TestAuditHistory
    protected def log(x: AuditEvent[TestEvent]) = copy(events = x :: events)

    type State = TestEvent
    type Message = TestEvent

    def state = events(0).evt
    protected def transition: (State, State, Long) => Option[This] = { (from, to, timestamp) =>
      Some(log(AuditEvent(to, timestamp)))
    }

    state(CreateTestEvent) { on =>
      on { case ModifyTestEvent => ModifyTestEvent -> transition }
    }

    state(ModifyTestEvent) { on =>
      on { case DeleteTestEvent => DeleteTestEvent -> transition }
    }
  }

  describe("Audit") {
    it("should create an audit instance and update state with events") {
      val msg = "I am a test"
      val history = TestHistory(msg)
      val event = AuditEvent[TestEvent](CreateTestEvent)
      val x = TestAuditHistory(history, List(event))

      x.subject.info should be(msg)
      x.events.size should be(1)
      x.state should be(CreateTestEvent)

      x.send(DeleteTestEvent) should be(None)

      x.send(ModifyTestEvent) map { m =>
        m.subject.info should be(msg)
        m.events.size should be(2)
        m.state should be(ModifyTestEvent)
      }
    }
  }

}

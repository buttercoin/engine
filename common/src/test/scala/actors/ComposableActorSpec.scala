package test.actors

import akka.actor._
import akka.pattern.ask
import akka.testkit.TestActorRef
import java.util.UUID
import org.buttercoin.common._
import org.buttercoin.common.actor._
import org.buttercoin.common.models.currency._
import org.buttercoin.common.models.money.CurrencyImplicits._
import org.scalatest._
import scala.concurrent.duration._
import scala.util.Success

trait FooActor extends ComposableActor {
  register {
    case 'Foo => sender ! 'Bar
    case 'Last => sender ! 'NO1
  }
}

trait PingActor extends ComposableActor {
  register {
    case 'Ping => sender ! 'Pong
    case 'Last => sender ! 'NO2
  }
}

class ComposableActorSpec extends FunSpec with BeforeAndAfter with Matchers {
  implicit val timeout = akka.util.Timeout(1000)
  implicit var system: ActorSystem = _ 
  var actorRef: TestActorRef[ComposableActor] = _

  before {
    system = ActorSystem("testSystem")
    actorRef = TestActorRef(new FooActor with PingActor {
      register {
        case 'Last => sender ! 'OK 
        case 'TestState => become("test")
      }

      registerState("test") { 
        case 'Test => sender ! 'OK
      }

      always { case 'Always => sender ! 'OK }
    })
  }

  after { system.shutdown() }

  describe("ComposableActor") {
    it("should be able to send messages to composed actors") {
      (actorRef ? 'Foo).value.get should be (Success('Bar))
      (actorRef ? 'Ping).value.get should be (Success('Pong))
    }

    it("should use the most recently registered handlers first") {
      (actorRef ? 'Last).value.get should be (Success('OK))
    }
    
    it("should not produce a result on an unhandled message") {
      (actorRef ? 'Fake).value should be (None)
    }

    it("should be able to switch states") {
      (actorRef ? 'Test).value should be (None)
      actorRef ! 'TestState
      (actorRef ? 'Test).value.get should be (Success('OK))
    }

    it("should respond to always cases in any state") {
      (actorRef ? 'Always).value.get should be (Success('OK))
      actorRef ! 'TestState
      (actorRef ? 'Always).value.get should be (Success('OK))
    }
  }
}

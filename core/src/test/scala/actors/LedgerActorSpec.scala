package test.actors

import akka.actor._
import akka.testkit.{ TestActorRef, TestKitBase }
import com.lmax.disruptor.{ util => _, _ }
import scala.language.implicitConversions
import org.scalatest.{ FunSpec, Matchers }
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalamock.scalatest.MockFactory
import org.buttercoin.engine.messages._
import org.buttercoin.common.messages._
import org.buttercoin.common.models.core.AccountID
import org.buttercoin.common.models.currency._
import org.buttercoin.common.models.money.CurrencyImplicits._
import org.buttercoin.engine.actors._
import org.buttercoin.engine.{ engine, EngineEvent }
import java.util.UUID
import concurrent.ExecutionContext.Implicits.global
import org.buttercoin.common.testhelper.Gen._
import scala.concurrent.duration._
import scala.language.postfixOps
import org.buttercoin.engine.fees.strategies.FeeStrategies

import scalaz._
import Scalaz._

class LedgerActorSpec extends FunSpec
                      with Matchers
                      with TestKitBase
                      with MockFactory
                      with GeneratorDrivenPropertyChecks
{
  implicit def self = testActor
  implicit lazy val system = ActorSystem("ledgerActorSpec")
  implicit def testActorRefObject[T <: Actor](testRef: TestActorRef[T]): T = testRef.underlyingActor

  def orderFailed(acct: AccountID, order: UUID): Unit = ()

  describe("LedgerActor") {
    it("should be able to perform a tracked deposit") {
      forAll { (acctId: AccountID, amt: USD) =>
        val m = mock[BufferAdapter]
        val ledger = TestActorRef(new LedgerActor(m, orderFailed, FeeStrategies.lookup))
        val exDeposit = LedgerDeposit(acctId, amt)
        val evt = EngineEvent()
        val balChanged = BalanceChanged(acctId, amt)
        (m.get(_)).expects(0L).returning(evt)
        (m.publish(_)).expects(0L)

        ledger ! Tracked(exDeposit, 0)

        val acct = ledger.accounts.get(acctId)
        acct shouldNot be ( 'empty )
        acct.get._1.balanceFor(amt).map(_.current) should be ( Some(amt) )

        evt.op should be ( engine.Nop )
        evt.updates should be {
          Success(
            List(
              OperationSuccess(balChanged),
              UserMessage(balChanged, acctId)))
        }
      }
    }

    it("should be able to perform a tracked withdrawal") {
      forAll { (acctId: AccountID, amt: USD) =>
        val m = mock[BufferAdapter]
        val ledger = TestActorRef(new LedgerActor(m, orderFailed, FeeStrategies.lookup))
        val exDeposit = LedgerDeposit(acctId, amt)
        val exWithdraw = LedgerWithdraw(acctId, amt)
        val evt = EngineEvent()
        val balChanged = BalanceChanged(acctId, usd(0))

        (m.get(_)).expects(*).twice.returning(evt)
        (m.publish(_)).expects(*).twice

        ledger ! Tracked(exDeposit, 0L)
        evt.updates = Success(List())
        ledger ! Tracked(exWithdraw, 1L)

        val acct = ledger.accounts.get(acctId)
        acct shouldNot be ( 'empty )
        acct.get._1.balanceFor(amt).map(_.current) should be ( Some(usd(0)) )

        evt.op should be ( engine.Nop )
        evt.updates should be {
          Success(
            List(
              OperationSuccess(balChanged),
              UserMessage(balChanged, acctId)))
        }

      }
    }

    it("should produce an error if a user has insufficient funds") {
      forAll { (acctId: AccountID, amt: USD) =>
        val m = mock[BufferAdapter]
        val ledger = TestActorRef(new LedgerActor(m, orderFailed, FeeStrategies.lookup))
        val exWithdraw = LedgerWithdraw(acctId, amt)
        val evt = EngineEvent()

        (m.get(_)).expects(*).returning(evt)
        (m.publish(_)).expects(*)

        ledger ! Tracked(exWithdraw, 0)

        evt.updates should be {
          scalaz.Failure(NonEmptyList("Insufficient funds"))
        }
      }
    }

    it("should be able to get balances for an existing account") {
      val m = mock[BufferAdapter]
      val ledger = TestActorRef(new LedgerActor(m, orderFailed, FeeStrategies.lookup))
      val evt = EngineEvent()
      val acctId = AccountID(UUID.randomUUID)
      val exDeposit = LedgerDeposit(acctId, usd("5"))

      (m.get(_)).expects(*).returning(evt)
      (m.publish(_)).expects(*)

      ledger ! Tracked(exDeposit, 0L)
      ledger ! GetBalances(acctId)

      val Balances(bals) = expectMsgClass(classOf[Balances])
      bals.contains( usd("5") ) should be ( true )
      within(20 milliseconds) { expectNoMsg() }
    }

    it("should produce balances with zero for a non-extant account") {
      pending
    }
  }
}

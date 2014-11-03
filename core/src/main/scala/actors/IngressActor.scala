package org.buttercoin.engine.actors

import akka.actor._
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import concurrent.ExecutionContext.Implicits.global
import org.buttercoin.common.messages._
import org.buttercoin.common.actor._
import org.buttercoin.engine._
import engine.{ STSNAP, LDSNAP }
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.concurrent.stm._
import org.buttercoin.engine.models.snapshot._
import org.buttercoin.common.models.core._

import scalaz._
import Scalaz._

final case class LedgerTotalRequest(ledgers: Ref[List[LedgerSnapshot]] = Ref(Nil), accountsBlacklist: List[AccountID] = List()) {
  import org.buttercoin.common.models.money.Currency
  import org.buttercoin.common.models.currency._
  import org.buttercoin.engine.models.Account

  val future = Future[TotalLedgerBalances] {
    atomic { implicit txn =>
      if(ledgers().length < 4) { retry }

      val sumAcct = Account(null)

      ledgers()
        .map(_.accounts).flatten
        .filter(acc => !accountsBlacklist.contains(acc._1))
        .map(_._2._1.currentBalances).flatten
        .foreach(sumAcct.deposit(_))
      TotalLedgerBalances(sumAcct.currentBalances)
    }
  }
}

class IngressActor(val buffer: BufferAdapter,
                   val ledgerRouter: ActorRef,
                   val engine: TradeEngine) extends Upstream with ActorLogging {
  implicit val timeout = Timeout(1000)

  register {
    case Tracked(msg: AccountOperation, seqNum) => {
      val next = buffer.next()
      val slot = buffer.get(next)
      slot.sanitize()
      slot.seqNum = seqNum
      ledgerRouter ! Tracked(msg, next)
    }

    case MessageRequest(msg: AccountOperation, Some(msgId)) =>
      (ledgerRouter ? Tracked(msg, buffer.next())).map { x =>
        MessageSuccess(x.asInstanceOf[Result], msgId)
      } recover { case err: Throwable =>
        MessageFailure(err.getMessage.point[List], msgId)
      } pipeTo sender

    case MessageRequest(msg: AccountQuery, Some(msgId)) =>
      (ledgerRouter ? msg).map { x =>
        MessageSuccess(x.asInstanceOf[Result], msgId)
      } recover { case err: Throwable =>
        MessageFailure(err.getMessage.point[List], msgId)
      } pipeTo sender

    case MessageRequest(msg: GetTicker, Some(msgId)) =>
      engine.tickers.foreach(sender ! MessageSuccess(_, msgId))

    case GetTotalLedgerBalances(accountsBlacklist) => {
      val x = LedgerTotalRequest(accountsBlacklist = accountsBlacklist)
      ledgerRouter ! akka.routing.Broadcast(x)
      x.future pipeTo sender
    }

    case x: EngineSnapshotRequest => {
      ledgerRouter ! akka.routing.Broadcast(x)
      atomic { implicit txn =>
        if(x.ledgers().length < 4) { retry }
      }
      val next = buffer.next()
      val slot = buffer.get(next)
      slot.sanitize
      slot.op = STSNAP(x)
      buffer.publish(next)
    }

    case EngineSnapshotOffer(snap) => {
      snap.ledgers foreach { l =>
        l.accounts foreach { acct =>
          ledgerRouter ! AccountSnapshotOffer(acct._1, acct._2._1, acct._2._2)
        }
      }

      val next = buffer.next()
      val slot = buffer.get(next)
      slot.sanitize
      slot.op = LDSNAP(snap.markets)
      buffer.publish(next)

      sender ! 'OK
    }

    case x =>
      log.error("Message is not an account operation: " + x)
  }
}

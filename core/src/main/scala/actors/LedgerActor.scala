package org.buttercoin.jersey.actors

import akka.actor.Actor
import org.buttercoin.common.fees._
import org.buttercoin.jersey._
import org.buttercoin.jersey.messages._
import org.buttercoin.common.messages._
import org.buttercoin.common.models.money._
import org.buttercoin.common.models.core.AccountID
import org.buttercoin.common.models.orderInfo.CreateOrder
import org.eligosource.eventsourced.core.Message
import org.buttercoin.jersey.models.Account
import org.buttercoin.jersey.models.snapshot._

import scala.concurrent.stm._

import scalaz._
import Scalaz._


class LedgerActor(val buffer: BufferAdapter,
                  val notifyOrderFailed: (AccountID, java.util.UUID) => Unit,
                  val strategies: Map[FeeStrategyID, StrategyFactory],
                  var accounts: Map[AccountID, (Account, FeeStrategy)] =
                    Map[AccountID, (Account, FeeStrategy)]()) extends Actor
{
  private def getOrCreateAccount(accountId: AccountID) = accounts.get(accountId) match {
    case Some(acct) => acct
    case None => {
      val acct = new Account(accountId)
      val strat = strategies.get(FeeStrategyID("default")).get()
      accounts = accounts + (accountId -> (acct, strat))
      (acct, strat)
    }
  }

  private def performDeposit(accountId: AccountID, amount: Currency): ValidationNel[String, List[RoutedResult]] = {
    val acct = getOrCreateAccount(accountId)._1
    acct.deposit(amount) match {
      case Some(newBalance) => List(UserMessage(BalanceChanged(accountId, newBalance), accountId)).successNel
      case None => s"Unsupported currency type".failureNel
    }
  }

  private def performWithdraw(accountId: AccountID, amount: Currency): ValidationNel[String, List[RoutedResult]] = {
    (for {
      // get the Account, ignore the fee strategy
      acct <- accounts.get(accountId).map(_._1)
      newBalance <- acct.withdraw(amount)
    } yield {
      List(UserMessage(BalanceChanged(accountId, newBalance), accountId)).successNel
    }) getOrElse {
      s"Insufficient funds".failureNel
    }
  }

  def performTradeCredit[T <: Currency](msg: CreditTrade[T]): ValidationNel[String, List[RoutedMessage]] = {
    val (acct, strat) = getOrCreateAccount(msg.accountId)
    val (credit, fee, rate) = strat.feeBreakdown(msg)

    acct.deposit(credit) match {
      case Some(newBalance) => {
        val feeMsg = FeeAssessed(msg.accountId, msg.orderId, credit, rate, fee, msg.completedAs)
        List(
          UserMessage(BalanceChanged(msg.accountId, newBalance), msg.accountId),
          ChannelMessage(feeMsg, "org.buttercoin.events.fees")
        ).successNel
      }
      case None => s"Error performing credit of $credit".failureNel
    }
  }

  def publish(op: engine.Op, updates: ValidationNel[String, List[RoutedMessage]], seqNum: Long) = {
    val event = buffer.get(seqNum)
    event.op = op
    event.updates = updates |+| event.updates
    buffer.publish(seqNum)
  }

  def useFeeStrategy(msg: UseFeeStrategy): ValidationNel[String, List[RoutedMessage]] = {
    val (acct, _) = getOrCreateAccount(msg.accountId)
    strategies.get(msg.strategyId) match {
      case Some(strat) => {
        accounts = accounts + (msg.accountId -> (acct, strat()))
        List(ChannelMessage(msg.strategyId, "org.buttercoin.events.fees.admin")).success
      }
      case None => s"Strategy ${msg.strategyId} not found".failureNel
    }
  }

  def handleTracked: PartialFunction[AccountOperation, (engine.Op, ValidationNel[String, List[RoutedMessage]])] = {
    case msg: LedgerDeposit => engine.Nop -> performDeposit(msg.accountId, msg.amount).map { xs =>
        OperationSuccess(xs.head.message) :: xs
    }
    case msg: LedgerWithdraw => engine.Nop -> performWithdraw(msg.accountId, msg.amount).map { xs =>
        OperationSuccess(xs.head.message) :: xs
    }
    case msg: CreateOrder =>
      val res = performWithdraw(msg.accountId, msg.info.offered)
      if (res.isSuccess) { engine.CO(msg) -> res }
      else {
        notifyOrderFailed(msg.info.accountId, msg.info.orderId)
        engine.Nop -> res
      }
    case msg: CancelOrder => engine.XO(msg) -> Nil.success
    case msg: CreditTrade[_] => engine.Nop -> performTradeCredit(msg)
    case msg: UseFeeStrategy => engine.Nop -> useFeeStrategy(msg)
  }

  def receive = {
    case Tracked(msg: AccountOperation, seqNum) => {
      val (op, updates) = handleTracked(msg)
      publish(op, updates, seqNum)
    }

    case GetBalances(accountId) => {
      val requestor = sender
      accounts.get(accountId).map(_._1) match {
        case Some(acct) => requestor ! Balances(acct.currentBalances)
        case None => requestor ! Balances(Nil)
      }
    }

    case req: JerseySnapshotRequest => atomic { implicit txn =>
      req.ledgers() = LedgerSnapshot(accounts.toList) :: req.ledgers()
    }

    case req: LedgerTotalRequest => atomic { implicit txn =>
      req.ledgers() = LedgerSnapshot(accounts.toList) :: req.ledgers()
    }

    case AccountSnapshotOffer(acctId, acct, strat) =>
      accounts = accounts + (acctId -> (acct, strat))
  }
}

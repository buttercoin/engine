package org.buttercoin.engine.messages

import com.typesafe.config.Config
import java.util.Date
import org.buttercoin.common.fees._
import org.buttercoin.common.messages._
import org.buttercoin.common.models.core._
import org.buttercoin.common.models.money._
import scala.collection.immutable.{ Set => ISet }

import scalaz._
import Scalaz._

case class OperationSuccess[T <: Result](message: T) extends RoutedMessage {
  type MessageType = T
}
case class ChannelMessage[T](message: T, channel: String) extends RoutedMessage {
  type MessageType = T
}
case class UseFeeStrategy(accountId: AccountID, strategyId: FeeStrategyID) extends AccountOperation

// Messages for internal operations to debit and credit accounts in the ledger
trait LedgerOperation extends AccountOperation { val amount: Currency }
final case class LedgerDeposit(accountId: AccountID, amount: Currency) extends LedgerOperation
final case class LedgerWithdraw(accountId: AccountID, amount: Currency) extends LedgerOperation


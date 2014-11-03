package org.buttercoin.engine.actors

import akka.actor.ActorRef
import org.buttercoin.engine.EngineEvent
import org.buttercoin.common.messages.AccountOperation
import org.buttercoin.common.models.core.AccountID
import org.buttercoin.common.models.order.OrderID


// Messaged used to inform executor to record a new engine actor
final case class AddEngine(node: ActorRef)

// Messages used by the order store
final case class NotifyOrderFailed(acctId: AccountID, orderId: OrderID)

// Messages that need to be journaled and added to ring buffer
@SerialVersionUID(1L)
final case class Tracked[+T <: AccountOperation](msg: T, sequenceNumber: Long)


// Interface used to access and modify contents of ring buffer
trait BufferAdapter {
  def get(n: Long): EngineEvent
  def publish(n: Long): Unit
  def next(): Long
  def remainingCapacity: Long
}


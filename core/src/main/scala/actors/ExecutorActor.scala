package org.buttercoin.engine.actors

import akka.actor._
import org.buttercoin.common.actor.SetUpstream
import org.buttercoin.common.messages._
import org.buttercoin.engine._
import org.buttercoin.engine.messages._

import scalaz._
import Scalaz._


class ExecutorActor extends Actor with ActorLogging {
  var engineSet = Set[ActorRef]()
  var senderMap = Map[Long, (MessageID, ActorRef)]()
  var upstream: ActorRef = null

  var nextSendSeq: Long = 0

  def remember(optId: Option[MessageID], requestor: ActorRef): Unit = {
    optId.map { x =>
      senderMap = senderMap + (nextSendSeq -> (x, requestor))
    }
  }

  def broadcast(msg: AccountOperation): Unit = {
    engineSet.foreach { x => x ! Tracked(msg, nextSendSeq) }
    nextSendSeq = nextSendSeq + 1
  }

  def respondTo(n: Long, resp: MessageID => MessageResponse): Unit = {
    senderMap.get(n) map { pair =>
      senderMap = senderMap - n
      pair._2 ! resp(pair._1)
    }
  }

  def accept(msg: EngineComplete) = {
    resolve(msg)
  }

  def resolve(msg: EngineComplete) = {
    msg.nextActions.foreach(self ! MessageRequest(_, None))
    msg.results match {
      case Failure(errs) => respondTo(msg.seqNum, MessageFailure(errs.list, _))
      case Success(xs) => xs.collect {
        case OperationSuccess(res) => respondTo(msg.seqNum, MessageSuccess(res, _))
        case x: RoutedMessage => upstream ! x
        case x => log.error("Unresolved message received by executor: " + x)
      }
    }
  }

  def receive = {
    case SetUpstream(u) => upstream = u
    case AddEngine(node) => engineSet = engineSet + node
    case 'Ready =>
      log.info("EXECUTOR IS READY FOR BUSINESS")
      context.become(execute)
  }

  def execute: Receive = {
    case MessageRequest(msg: AccountOperation, optId) => {
      remember(optId, sender)
      broadcast(msg)
    }
    case msg: EngineComplete =>
      accept(msg)
  }
}

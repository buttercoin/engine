package org.buttercoin.engine

import akka.actor.{ Actor, ActorRef, ActorLogging }
import org.buttercoin.common.messages._
import org.buttercoin.common.actor.SetUpstream
import org.buttercoin.engine.messages._

class FeeManagerActor(val executor: ActorRef) extends Actor with ActorLogging {
  var upstream: ActorRef = null
  def receive = {
    case SetUpstream(u) => {
      upstream = u
      upstream ! RegisterForChannel(self, "org.buttercoin.events.fees")
    }
    case msg: FeeAssessed => {
      log.debug("Evaluating fee assessment: " + msg)
      if(msg.feeAmount.amount > 0) {
        upstream ! ChannelMessage(msg, "org.buttercoin.archive")
      }
    }
  }
}

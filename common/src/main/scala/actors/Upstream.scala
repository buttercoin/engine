package org.buttercoin.common.actor

import akka.actor.{ ActorRef, ActorLogging }

/** Sent to indicate the upstream actor for an {@link Upstream} instance */
case class SetUpstream(upstream: ActorRef)

/** Allows the specification of an upstream actor, used to
 *   route events or messages to an interested third party */
trait Upstream extends ComposableActor with ActorLogging {
  protected val alwaysSetUpstream: Boolean = false
  var upstream = context.parent

  def onSetUpstream(newRef: ActorRef, oldRef: ActorRef): Unit = ()

  private def handleSetUpstream: Receive = {
    case SetUpstream(u) => {
      log.debug(self.path + " setting upstream to: " + u.path)
      onSetUpstream(u, upstream)
      upstream = u
    }
  }

  if(alwaysSetUpstream) {
    always(handleSetUpstream)
  }
  else {
    register(handleSetUpstream)
  }
}

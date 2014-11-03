package org.buttercoin.engine

import org.buttercoin.common.messages._
import org.buttercoin.common.actor._
import akka.actor._
import scala.collection.mutable.{ HashMap, MultiMap, Set }

trait ChannelManager extends ComposableActor with ActorLogging {
  val channelMap = new HashMap[String, Set[ActorRef]] with MultiMap[String, ActorRef]
  val actorMap = new HashMap[ActorRef, Set[String]] with MultiMap[ActorRef, String]

  always {
    case RegisterForBroadcast(actor) =>
      registerForChannel("broadcast")(actor)
    case UnregisterForBroadcast(actor) =>
      unregisterForChannel("broadcast")(actor)
    case RegisterForChannel(actor, channel) =>
      registerForChannel(channel)(actor)
    case UnregisterForChannel(actor, channel) =>
      unregisterForChannel(channel)(actor)
    case Terminated(actor) =>
      log.info(s"Unregistering for all channels - ${actor.path}")
      val bindings = actorMap.get(actor).getOrElse(Set())
      actorMap.remove(actor)
      bindings foreach { x =>
        channelMap.removeBinding(x, actor)
      }
  }

  protected def sendToChannel(channel: String)(msg: Any): Unit =
    channelMap.get(channel).map { xs =>
      xs.foreach { _ ! msg }
    }

  private def registerForChannel(channel: String)(actor: ActorRef): Unit = {
    log.info(s"Registering for [ $channel ] - ${actor.path}")
    channelMap.addBinding(channel, actor)
    actorMap.addBinding(actor, channel)

    context.watch(actor)
  }

  private def unregisterForChannel(channel: String)(actor: ActorRef): Unit = {
    log.info(s"Unregistering for [ $channel ] - ${actor.path}")
    channelMap.removeBinding(channel, actor)
    actorMap.removeBinding(actor, channel)
  }
}

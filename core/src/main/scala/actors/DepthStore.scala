package org.buttercoin.engine.actors

import org.buttercoin.common.messages._
import org.buttercoin.common.actor.Upstream
import org.buttercoin.common.models.currency._
import org.buttercoin.engine.models.market.depth

import akka.actor.ActorRef

trait DepthStore extends Upstream {
  var markets = depth.Store()

  override def onSetUpstream(newRef: ActorRef, oldRef: ActorRef) = {
    oldRef ! UnregisterForChannel(self, "org.buttercoin.market.depth")
    newRef ! RegisterForChannel(self, "org.buttercoin.market.depth")
  }

  register {
    case DepthChange(parity, price, qty) =>
      val (newQ, newMkt) = markets.add(parity, price, qty)
      markets = newMkt
      upstream ! BroadcastMessage(MarketDepth(parity, price, newQ.getOrElse(qty.factory.zero)))

    case MarketDepth(parity, price, qty) =>
      markets = markets.set(parity, price, qty)
      upstream ! BroadcastMessage(MarketDepth(parity, price, qty))

    case GetMarketDepth(priceCode, qtyCode) =>
      sender ! ResultSeq {
        depth.marketL(priceCode -> qtyCode)
          .get(markets).view.toSeq
          .map { x => MarketDepth(x._1._1, x._1._2, x._2) }
      }
  }
}


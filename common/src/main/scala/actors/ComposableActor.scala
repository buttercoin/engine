package org.buttercoin.common.actor

import akka.actor.{ Actor, ActorLogging }
import collection.mutable.Queue

/**
  * Enables simple composition of actor message handling states. Multiple
  * `register` handlers can be defined for a given state name and they will be
  * tried in turn until one matches.
  *
  * ==Example==
  * <pre>
  * <code>
  * class GreetingActor extends ComposableActor {
  *   register {
  *     case "Hello" =&gt; {
  *       println("World")
  *       become("goodbye")
  *     }
  *   }
  *
  *   registerState("goodbye") {
  *     case "Hello" =&gt; {
  *       println("Goodbye")
  *       become("receive")
  *     }
  *   }
  * }
  * </code>
  * </pre>
 **/
trait ComposableActor extends Actor with ActorLogging {
  type HandlerQueue = Queue[Receive]

  // Actor is initially in 'receive' state awaiting incoming messages
  private var curState = "receive"
  private var states = Map[String, HandlerQueue]("receive" -> new HandlerQueue())
  private val alwaysQueue = new HandlerQueue()

  // Initially the handlerFn is for that of an Actor in the receive state
  private var handlerFn = mkHandlerFn(states.get("receive").get)
  private def mkHandlerFn(handlers: HandlerQueue): Option[Receive] = {
    val xs = handlers ++ alwaysQueue
    if (xs.size > 0) { Some(xs.reduceLeft(_ orElse _)) }
    else None
  }

  /** Actor mailbox handler which dispatches to `register` and `registerState` handlers */
  def receive = {
    case x if handlerFn.map(_.isDefinedAt(x)).getOrElse(false) => {
      (handlerFn.get)(x)
    }
    case x => log.debug(this.getClass.getName +
        "@[" + self.path + "] got unknown message " + x + " in state: " + curState)
  }

  /** Convenient shorthand for `registerState("receive")`, which is the default state. */
  def register(x: Receive) = { registerState("receive")( x ) } // +=: prepends to the handlers queue

  /** Register a handler for a given `state`. If multiple handlers are
   * registered for the same state, they will be tried in order until on is
   * able to handle the message. */
  def registerState(state: String)(x: Receive) = {
    val h = states get(state) getOrElse(new HandlerQueue())
    h.+=:(x)
    states += state -> h
    if (curState == state) {
      handlerFn = mkHandlerFn(h)
    }
  }

  def always(x: Receive) = {
    alwaysQueue.+=:(x)
    val h = states get(curState) getOrElse(new HandlerQueue())
    handlerFn = mkHandlerFn(h)
  }

  /** Switches to a handler set specified by `state`. Unlike the standard actor
   * become, there is no stack mechanism. */
  def become(state: String) = {
    curState = state
    states get(state) map { x => handlerFn = mkHandlerFn(x) } getOrElse {
      context.system.stop(self)
    }
  }
}


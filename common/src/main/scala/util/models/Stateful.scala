package org.buttercoin.common.util

import java.util.Date
import scala.reflect.ClassTag

trait Stateful {
  type State
  type Message
  type This
  type TxFunction = PartialFunction[Message, (State, (State, State, Long) => Option[This])]

  def state: State

  private var transitions = Map[State, TxFunction]()
  private var typeTransitions = Map[Class[_], TxFunction]()

  trait TransitionBuilder {
    val curState: State

    def apply(f: TxFunction) = {
      transitions = transitions + (curState -> f)
    }
  }

  trait TypeTransitionBuilder {
    val curState: Class[_]
    def apply(f: TxFunction) = {
      typeTransitions = typeTransitions + (curState -> f)
    }
  }

  def state(label: State)(f: TransitionBuilder => Unit) = f {
    new TransitionBuilder { val curState = label }
  }

  def state[T <: State : ClassTag](f: TypeTransitionBuilder => Unit) = f {
     new TypeTransitionBuilder { val curState = implicitly[ClassTag[T]].runtimeClass }
  }

  protected def transition: (State, State, Long) => Option[This]

  def send(msg: Message, timestamp: Long = new Date().getTime()): Option[This] =
    transitions.get(state)
      .orElse(typeTransitions.get(state.getClass()))
      .filter(_.isDefinedAt(msg))
      .flatMap { txMap =>
        val (newS, action) = txMap(msg)
        action(state, newS, timestamp)
      }
}


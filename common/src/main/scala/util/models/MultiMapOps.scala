package org.buttercoin.common.util

import collection.immutable.{ Map, Set }

object MultiMapOps {
  implicit class BindingOps[A, B](val map: Map[A, Set[B]]) extends AnyVal {
    def addBinding(k: A, v: B): Map[A, Set[B]] = {
      val set = map.get(k) match {
        case None => Set(v)
        case Some(x) => x + v
      }
      map + (k -> set)
    }

    def removeBinding(k: A, v: B): Map[A, Set[B]] = {
      map.get(k) match {
        case None => map
        case Some(set) => map + (k -> (set - v))
      }
    }
  }
}


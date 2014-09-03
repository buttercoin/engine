package org.buttercoin.common.util.config

import com.typesafe.config.Config
import scala.collection.JavaConversions._

object fallback {
  implicit class ConfigWithFallback(val cfg: Config) extends AnyVal {
    def getStringListWithFallback(name: String): List[String] = {
      val result = cfg.getStringList(name)
      result.isEmpty match {
        case false => result.toList
        case true => cfg.getStringList(s"fallback-$name").toList
      }
    }
  }
}

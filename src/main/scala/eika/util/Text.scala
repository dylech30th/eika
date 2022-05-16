package ink.sora
package eika.util

import scala.reflect.runtime.universe.{Constant, Literal}

extension (s: String)
  def escaped(): String = {
    Literal(Constant(s)).toString
  }
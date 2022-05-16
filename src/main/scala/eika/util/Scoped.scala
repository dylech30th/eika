package ink.sora
package eika.util

import scala.annotation.targetName

extension [T](x: T)
  def let[R](f: T => R): R = f(x)
  def also(f: T => Unit): T = { f(x); x }
  
  @targetName("pipe")
  def |>[Z](f: T => Z) = f(x)
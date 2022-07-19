package ink.sora
package eika.util

def repeat(times: Int, action: Int => Unit): Unit =
  for i <- 0 until times do action(i)

def repeat[T](times: Int, action: Int => T): IndexedSeq[T] =
  for i <- 0 until times yield action(i)
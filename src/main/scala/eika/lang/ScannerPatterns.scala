package ink.sora
package eika.lang

trait Ascertainable[T]:
  def ascertain(subject: T): Boolean
end Ascertainable

object IdentifierStart extends Ascertainable[Char]:
  def unapply(c: Char): Option[Char] = Some(c).filter(Character.isJavaIdentifierStart)
  def ascertain(subject: Char): Boolean = unapply(subject).isDefined

object IdentifierPart extends Ascertainable[Char]:
  def unapply(c: Char): Option[Char] = Some(c).filter(Character.isJavaIdentifierPart)
  def ascertain(subject: Char): Boolean = unapply(subject).isDefined

object Number extends Ascertainable[Char]:
  def unapply(c: Char): Option[Char] = Some(c).filter(Character.isDigit)
  def ascertain(subject: Char): Boolean = unapply(subject).isDefined
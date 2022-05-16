package ink.sora
package eika.lang

case class LineInfo(lineNumber: Int, columnNumber: Int) {
  override def toString: String = s"$lineNumber:$columnNumber"
}
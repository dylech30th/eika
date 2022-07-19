package ink.sora
package eika.lang

import eika.util.also

import scala.annotation.tailrec
import scala.collection.mutable

class WhitespaceTransformationPass extends LexerPass:
  case class MarkedToken(token: Token, markedAsDelete: Boolean)

  override def transform(input: List[Token]): List[Token] =
    val normalized = removeLeadingTrailingWhitespaces(input)
    val positions = mark(normalized)
    normalized.map(MarkedToken(_, false)).toBuffer.also { buffer =>
      buffer.zipWithIndex.foreach {
        case (MarkedToken(_, markedAsDelete), i) if !markedAsDelete =>
          if positions contains i then
            (1 to 3).foreach(offset => buffer.update(i + offset, buffer(i + offset).copy(markedAsDelete = true)))
        case _ => ()
      }
    }.zipWithIndex.map {
      case (MarkedToken(token @ Token(_, TextWindow(_, range, (lineInfoStart, LineInfo(line, col)))), markedAsDelete), index) if positions contains index =>
        val newRange = range.start to range.end + 3
        val newLineInfo = (lineInfoStart, LineInfo(line, col + 3))
        MarkedToken(token.copy(TokenKind.TriviaTab, TextWindow("\t", newRange, newLineInfo)), markedAsDelete)
      case (token, _) => token
    }.filterNot(_.markedAsDelete).map(_.token).toList
  end transform

  @tailrec
  private def removeLeadingTrailingWhitespaces(input: List[Token]): List[Token] =
    input match
      case Token(TokenKind.TriviaWhitespace, _) :: tail => removeLeadingTrailingWhitespaces(tail)
      case init :+ Token(TokenKind.TriviaWhitespace, _) => removeLeadingTrailingWhitespaces(init)
      case token => token
  end removeLeadingTrailingWhitespaces

  private def mark(input: List[Token]): Set[Int] =
    var counter = 0
    val positions = mutable.ListBuffer[Int]()
    for (Token(kind, _), index) <- input.zipWithIndex do
      kind match
        case TokenKind.TriviaWhitespace =>
          if counter == 3 then
            positions += (index - 3)
            counter = 0
          else
            counter += 1
        case _ => counter = 0
    positions.toSet
end WhitespaceTransformationPass

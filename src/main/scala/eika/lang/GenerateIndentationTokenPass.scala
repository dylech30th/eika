package ink.sora
package eika.lang

import scala.collection.mutable
import scala.collection.immutable.Queue
import scala.language.postfixOps
import eika.util.repeat

class GenerateIndentationTokenPass extends LexerPass:
  override def transform(input: List[Token]): List[Token] = generateTokens(input)

  private def generateTokens(input: List[Token]): List[Token] =
    val lines = input.foldLeft(List[mutable.ListBuffer[Token]](mutable.ListBuffer()))((acc, t) => t match
      case token @ Token(TokenKind.TriviaNewline(_), _) =>
        acc.last += token
        acc.appended(mutable.ListBuffer[Token]())
      case t =>
        acc.last += t
        acc
    )
    var lastLineIndentation = 0
    val generatedTokens = mutable.ListBuffer[Token]()
    for line <- lines do
      val leadingTabs = line.takeWhile(_.kind.isInstanceOf[TokenKind.TriviaTab.type])
      val remains = line.drop(leadingTabs.size)
      if leadingTabs.size > lastLineIndentation then
        generatedTokens ++= repeat(leadingTabs.size - lastLineIndentation, index => Token(TokenKind.Indent, leadingTabs(index).textWindow))
      else if leadingTabs.size < lastLineIndentation then
        generatedTokens ++= repeat(lastLineIndentation - leadingTabs.size, index => Token(TokenKind.Dedent, TextWindow.dummy))
      lastLineIndentation = leadingTabs.size
      generatedTokens ++= remains
    generatedTokens.toList
end GenerateIndentationTokenPass

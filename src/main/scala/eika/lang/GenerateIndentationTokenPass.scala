package ink.sora
package eika.lang

import scala.collection.mutable
import scala.collection.immutable.Queue
import scala.language.postfixOps

class GenerateIndentationTokenPass extends LexerPass:
  def generateTokens(input: List[Token]): List[Token] =
    var lastLineIndent = 0 // inelegant, but works
    val lines = input.foldLeft(Queue[mutable.ListBuffer[Token]](mutable.ListBuffer()))((acc, t) => t match
      case token @ Token(TokenKind.TriviaNewline(_), _) =>
        acc.last.addOne(token)
        acc.enqueue(mutable.ListBuffer())
      case t =>
        acc.last += t
        acc
    )

end GenerateIndentationTokenPass

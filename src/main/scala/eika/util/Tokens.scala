package ink.sora
package eika.util

import eika.lang.Token
import eika.lang.TokenKind

extension (stream: List[Token])
  def nonTrivial(): List[Token] =
    stream.filterNot {
      case Token(TokenKind.TriviaNewline(_), _) | Token(TokenKind.TriviaWhitespace, _) | Token(TokenKind.TriviaTab, _) => true
      case _ => false
    }
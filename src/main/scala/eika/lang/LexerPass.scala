package ink.sora
package eika.lang

trait LexerPass:
  def transform(input: List[Token]): List[Token]
end LexerPass
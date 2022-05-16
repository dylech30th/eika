package ink.sora
package eika.lang

import eika.util.also

import scala.annotation.tailrec

final class Postprocessor private (private val tokens: List[Token], private val passes: List[LexerPass]):
  def applyTransformation(): List[Token] = applyTransformationStage()

  private def applyTransformationStage(): List[Token] =
    passes.foldLeft(tokens)((tokens, pass) => pass.transform(tokens))

  @tailrec
  private def validate(tokens: List[Token]): Unit =
    tokens match
      case token @ Token(kind, _) :: tail => kind.species match
        case TokenSpecies.SyntaxIntrinsic =>
          throw new ScannerException(s"Syntax intrinsic token is not allowed after the preprocessing stage: Token $token")
        case _ => validate(tail)
      case Nil => ()
end Postprocessor

object Postprocessor:
  def apply(tokens: List[Token], passes: List[LexerPass]): Postprocessor = new Postprocessor(tokens, passes)
end Postprocessor

package ink.sora
package eika.lang

import eika.util.let
import eika.util.escaped
import eika.util.nonTrivial

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.Using

final class Scanner(val sourceText: SourceText):
  import eika.lang.Scanner.keywordTokenKindSet

  def all(): List[Token] =
    sourceText.reset()
    def loop(): List[Token] =
      next() match
        case Some(token) =>
          token :: loop()
        case None => Nil
    loop()
  end all

  def next(): Option[Token] =
    for
      ch <- sourceText.current()
      token <- ch match
        case IdentifierStart(_) => scanIdentifierOrKeyword()
        case Number(_) => scanNumber()
        case ' ' => sourceText.one().map(Token(TokenKind.TriviaWhitespace, _))
        case '\t' => sourceText.one().map(Token(TokenKind.TriviaTab, _))
        case '\r' => scanReset()
        case '\n' => sourceText.one().map(Token(TokenKind.TriviaNewline(false), _))
        case ',' => sourceText.one().map(Token(TokenKind.Comma, _))
        case ':' => scanColon()
        case ';' => sourceText.one().map(Token(TokenKind.Semicolon, _))
        case '+' => sourceText.one().map(Token(TokenKind.Plus, _))
        case '-' => scanSlash()
        case '*' => sourceText.one().map(Token(TokenKind.Multiplication, _))
        case '/' => sourceText.one().map(Token(TokenKind.Division, _))
        case '%' => sourceText.one().map(Token(TokenKind.Modulo, _))
        case '=' => scanEqual()
        case '!' => scanNot()
        case '>' => scanGreater()
        case '<' => scanLess()
        case '\'' => scanApostrophe()
        case '"' => scanQuote()
        case '&' => scanAmpersand()
        case '|' => scanPipe()
        case '^' => sourceText.one().map(Token(TokenKind.Caret, _))
        case '(' => sourceText.one().map(Token(TokenKind.LeftParen, _))
        case ')' => sourceText.one().map(Token(TokenKind.RightParen, _))
        case '[' => scanLeftBracket()
        case ']' => scanRightBracket()
        case '{' => scanLeftBrace()
        case '}' => scanRightBrace()
        case '.' => scanDot()
        case '@' => sourceText.one().map(Token(TokenKind.At, _))
        case '?' => sourceText.one().map(Token(TokenKind.Question, _))
        case _ => reportError(sourceText.current(), "Unexpected character")
    yield token
  end next

  private def scanIdentifierOrKeyword(): Option[Token] =
    sourceText.advance()
    sourceText.advanceWhile(IdentifierPart.ascertain)
    sourceText.windowAndCatchUp().map { window =>
      keywordTokenKindSet.find(_.content == window.content)
        .map(Token(_, window))
        .getOrElse(Token(TokenKind.Identifier(window.content), window))
    }.map {
      case Token(TokenKind.KeywordTrue, trueTextWindow) => Token(TokenKind.BoolLiteral(true), trueTextWindow)
      case Token(TokenKind.KeywordFalse, falseTextWindow) => Token(TokenKind.BoolLiteral(false), falseTextWindow)
      case t => t
    }
  end scanIdentifierOrKeyword

  private def scanNumber(): Option[Token] =
    sourceText.advanceWhile(Number.ascertain)
    sourceText.pioneer() match
      case Some('.') =>
        sourceText.advanceAndGet() match
          case Some(c) if '0' to '9' contains c =>
            sourceText.advanceWhile(Number.ascertain)
            sourceText.windowAndCatchUp().map(window => Token(TokenKind.DoubleLiteral(window.content.toDouble), window))
          case _ =>
            sourceText.ret()
            sourceText.windowAndCatchUp().map(window => Token(TokenKind.IntLiteral(window.content.toInt), window))
      case _ => sourceText.windowAndCatchUp().map(window => Token(TokenKind.IntLiteral(window.content.toInt), window))
  end scanNumber

  private def scanReset(): Option[Token] =
    sourceText.advance()
    sourceText.pioneer() match
      case Some('\n') =>
        sourceText.advance()
        sourceText.windowAndCatchUp().map(Token(TokenKind.TriviaNewline(true), _))
      case _ => reportError(sourceText.pioneer(), '\n')
  end scanReset

  private def scanColon(): Option[Token] =
    sourceText.advance()
    sourceText.pioneer() match
      case Some(':') =>
        sourceText.advance()
        sourceText.windowAndCatchUp().map(Token(TokenKind.ColonColon, _))
      case _ => sourceText.windowAndCatchUp().map(Token(TokenKind.Colon, _))
  end scanColon

  private def scanSlash(): Option[Token] =
    sourceText.advance()
    sourceText.pioneer() match
      case Some('>') =>
        sourceText.advance()
        sourceText.windowAndCatchUp().map(Token(TokenKind.Arrow, _))
      case _ => sourceText.windowAndCatchUp().map(Token(TokenKind.Minus, _))
  end scanSlash

  private def scanEqual(): Option[Token] =
    sourceText.advance()
    sourceText.pioneer() match
      case Some('=') =>
        sourceText.advance()
        sourceText.windowAndCatchUp().map(Token(TokenKind.EqualEqual, _))
      case _ => sourceText.windowAndCatchUp().map(Token(TokenKind.Equal, _))
  end scanEqual

  private def scanNot(): Option[Token] =
    sourceText.advance()
    sourceText.pioneer() match
      case Some('=') =>
        sourceText.advance()
        sourceText.windowAndCatchUp().map(Token(TokenKind.NotEqual, _))
      case _ => sourceText.windowAndCatchUp().map(Token(TokenKind.Not, _))
  end scanNot

  private def scanGreater(): Option[Token] =
    sourceText.advance()
    sourceText.pioneer() match
      case Some('=') =>
        sourceText.advance()
        sourceText.windowAndCatchUp().map(Token(TokenKind.GreaterEqual, _))
      case _ => sourceText.windowAndCatchUp().map(Token(TokenKind.Greater, _))
  end scanGreater

  private def scanLess(): Option[Token] =
    sourceText.advance()
    sourceText.pioneer() match
      case Some('=') =>
        sourceText.advance()
        sourceText.windowAndCatchUp().map(Token(TokenKind.LessEqual, _))
      case _ => sourceText.windowAndCatchUp().map(Token(TokenKind.Less, _))
  end scanLess

  private def scanApostrophe(): Option[Token] =
    def charLiteralHelper(): Option[Token] =
      sourceText.advance() // consume '
      sourceText.pioneer() match
        case Some('\\') =>
          sourceText.advance() // consume \
          if sourceText.pioneer().exists(isEscapableCharacter) then
            sourceText.advance() // consume escaped character
            if sourceText.pioneer().contains('\'') then
              sourceText.advance() // consume '
              val window = sourceText.windowAndCatchUp().get
              val escapedCharacter = window.content(2) match
                case 'n' => '\n'
                case 't' => '\t'
                case 'r' => '\r'
                case 'b' => '\b'
                case '\\' => '\\'
                case '\'' => '\''
                case '"' => '"'
              Some(Token(TokenKind.CharLiteral(escapedCharacter), window))
            else reportError(sourceText.pioneer(), '\'')
          else reportError(sourceText.pioneer(), "Expecting an escapable character")
        case Some(c) =>
          sourceText.advance() // consume character
          if sourceText.pioneer().contains('\'') then
            sourceText.advance() // consume '
            sourceText.windowAndCatchUp().map(Token(TokenKind.CharLiteral(c), _))
          else
            sourceText.advanceWhile(('a' to 'z') ++ ('A' to 'Z') contains _)
            sourceText.windowAndCatchUp().map(window => Token(TokenKind.GenericLiteral(window.content), window))
        case _ => reportError(sourceText.peek(), '\'')
    charLiteralHelper()
  end scanApostrophe

  private def scanQuote(): Option[Token] =
    sourceText.advance() // consume "
    def consumeAllChar(): String =
      sourceText.pioneer() match
        case Some('\\') =>
          sourceText.advance() // consume \
          if sourceText.pioneer().exists(isEscapableCharacter) then
            val escapedCharacter = sourceText.getAndAdvance() match // consume escaped character
              case Some('n') => '\n'
              case Some('t') => '\t'
              case Some('r') => '\r'
              case Some('b') => '\b'
              case Some('\\') => '\\'
              case Some('\'') => '\''
              case Some('"') => '"'
              case _ => reportError(sourceText.pioneer(), "Expecting an escapable character")
            escapedCharacter + consumeAllChar()
          else reportError(sourceText.pioneer(), "Expecting an escapable character")
        case Some('\"') =>
          sourceText.advance() // consume "
          ""
        case Some(c) =>
          sourceText.advance() // consume character
          c + consumeAllChar()
        case _ => reportError(sourceText.pioneer(), '"')
    val content = consumeAllChar()
    sourceText.windowAndCatchUp().map(Token(TokenKind.StringLiteral(content), _))
  end scanQuote

  private def scanAmpersand(): Option[Token] =
    sourceText.advance() // consume &
    sourceText.pioneer() match
      case Some('&') =>
        sourceText.advance() // consume &
        sourceText.windowAndCatchUp().map(Token(TokenKind.AmpAmp, _))
      case _ => sourceText.windowAndCatchUp().map(Token(TokenKind.Amp, _))
  end scanAmpersand

  private def scanPipe(): Option[Token] =
    sourceText.advance() // consume |
    sourceText.pioneer() match
      case Some('|') =>
        sourceText.advance() // consume |
        sourceText.windowAndCatchUp().map(Token(TokenKind.PipePipe, _))
      case _ => sourceText.windowAndCatchUp().map(Token(TokenKind.Pipe, _))
  end scanPipe

  private def scanLeftBracket(): Option[Token] =
    sourceText.advance() // consume [
    sourceText.pioneer() match
      case Some('|') =>
        sourceText.advance() // consume |
        sourceText.windowAndCatchUp().map(Token(TokenKind.LeftBracketPipe, _))
      case _ => sourceText.windowAndCatchUp().map(Token(TokenKind.LeftBracket, _))
  end scanLeftBracket

  private def scanRightBracket(): Option[Token] =
    sourceText.advance() // consume ]
    sourceText.pioneer() match
      case Some('|') =>
        sourceText.advance() // consume |
        sourceText.windowAndCatchUp().map(Token(TokenKind.RightBracketPipe, _))
      case _ => sourceText.windowAndCatchUp().map(Token(TokenKind.RightBracket, _))
  end scanRightBracket

  private def scanLeftBrace(): Option[Token] =
    sourceText.advance() // consume {
    sourceText.pioneer() match
      case Some('|') =>
        sourceText.advance() // consume |
        sourceText.windowAndCatchUp().map(Token(TokenKind.LeftBracePipe, _))
      case _ => sourceText.windowAndCatchUp().map(Token(TokenKind.LeftBrace, _))
  end scanLeftBrace

  private def scanRightBrace(): Option[Token] =
    sourceText.advance() // consume }
    sourceText.pioneer() match
      case Some('|') =>
        sourceText.advance() // consume |
        sourceText.windowAndCatchUp().map(Token(TokenKind.RightBracePipe, _))
      case _ => sourceText.windowAndCatchUp().map(Token(TokenKind.RightBrace, _))
  end scanRightBrace

  private def scanDot(): Option[Token] =
    sourceText.advance() // consume .
    sourceText.pioneer() match
      case Some('.') =>
        sourceText.advance() // consume .
        sourceText.windowAndCatchUp().map(Token(TokenKind.DotDot, _))
      case _ => sourceText.windowAndCatchUp().map(Token(TokenKind.Dot, _))
  end scanDot

  private def isEscapableCharacter(c: Char): Boolean =
    Set('"', '\'', '\\', 'n', 'r', 't', 'b').contains(c)

  private def reportError(c: Option[Char], expectation: Char): Nothing =
    throw new ScannerException(s"Expecting ${expectation.toString.escaped()} at ${sourceText.position()}, but ${c.map(_.toString).getOrElse("EOF")} was found.")

  private def reportError(c: Option[Char], reason: String): Nothing =
    throw new ScannerException(s"$reason, but ${c.map(_.toString).getOrElse("EOF")} was found.")
end Scanner

object Scanner:
  val keywordTokenKindSet: Set[TokenKind] = Set(
    TokenKind.KeywordLet,
    TokenKind.KeywordIf,
    TokenKind.KeywordElse,
    TokenKind.KeywordThen,
    TokenKind.KeywordWhile,
    TokenKind.KeywordFor,
    TokenKind.KeywordIn,
    TokenKind.KeywordMatch,
    TokenKind.KeywordAs,
    TokenKind.KeywordFun,
    TokenKind.KeywordType,
    TokenKind.KeywordDo,
    TokenKind.KeywordWith,
    TokenKind.KeywordImport,
    TokenKind.KeywordModule,
    TokenKind.KeywordAsync,
    TokenKind.KeywordAwait,
    TokenKind.KeywordYield,
    TokenKind.KeywordOf,
    TokenKind.KeywordTrue,
    TokenKind.KeywordFalse
  )
end Scanner

@main
def main(): Unit =
  for scanner <- Using[Source, Scanner](Source.fromFile("C:\\Users\\26532\\OneDrive\\桌面\\eika-lexer-test.txt"))(source => Scanner(source.mkString)) do
    Postprocessor(scanner.all(), WhitespaceTransformationPass() :: Nil).applyTransformation().foreach(println)
end main
package ink.sora
package eika.lang

import eika.util.escaped

case class Token(kind: TokenKind, textWindow: TextWindow):
  override def toString: String = s"Token($kind, TextWindow(${textWindow.content.escaped()}, ${textWindow.range.start}..${textWindow.range.end}, ${textWindow.rangeLineInfo}))"
end Token

// Defines the species of a token
// SyntaxStruct: the token can be used as the concrete placeholder of a code snippet in the final token flow
//               after all preprocessor passes
// SyntaxTrivia: the token can be used as the concrete placeholder of a code snippet in the final token flow
//               after all preprocessor passes, but it won't present in the abstract syntax tree
// SyntaxIntrinsic: the token will be used internally in the lexical analysis phase, but won't live any longer
//                  after all preprocessor passes, i.e., it will not present in the final token flow
// SyntaxReserved: same as SyntaxStruct, except that these tokens have no semantics during the syntax analysis phase
//                 , the are preserved to be utilized in the future
enum TokenSpecies:
  case SyntaxStruct extends TokenSpecies
  case SyntaxTrivia extends TokenSpecies
  case SyntaxIntrinsic extends TokenSpecies
  case SyntaxReserved extends TokenSpecies
end TokenSpecies

enum TokenKind(val content: String, val species: TokenSpecies = TokenSpecies.SyntaxStruct):
  case KeywordLet extends TokenKind("let")
  case KeywordIf extends TokenKind("if")
  case KeywordElse extends TokenKind("else")
  case KeywordThen extends TokenKind("then")
  case KeywordWhile extends TokenKind("while")
  case KeywordFor extends TokenKind("for")
  case KeywordIn extends TokenKind("in")
  case KeywordMatch extends TokenKind("match")
  case KeywordAs extends TokenKind("as")
  case KeywordFun extends TokenKind("fun")
  case KeywordType extends TokenKind("type")
  case KeywordDo extends TokenKind("do")
  case KeywordWith extends TokenKind("with")
  case KeywordImport extends TokenKind("import")
  case KeywordModule extends TokenKind("module")
  case KeywordStatic extends TokenKind("static")
  case KeywordMember extends TokenKind("member")
  case KeywordAsync extends TokenKind("async", TokenSpecies.SyntaxReserved)
  case KeywordAwait extends TokenKind("await", TokenSpecies.SyntaxReserved)
  case KeywordYield extends TokenKind("yield", TokenSpecies.SyntaxReserved)
  case KeywordOf extends TokenKind("of")
  case KeywordTrue extends TokenKind("true", TokenSpecies.SyntaxIntrinsic)
  case KeywordFalse extends TokenKind("false", TokenSpecies.SyntaxIntrinsic)
  case TriviaNewline(val leadingReset: Boolean) extends TokenKind(if (leadingReset) "\r\n" else "\n", TokenSpecies.SyntaxTrivia)
  case TriviaTab extends TokenKind("\t", TokenSpecies.SyntaxIntrinsic)
  case TriviaWhitespace extends TokenKind(" ", TokenSpecies.SyntaxTrivia)
  case Comma extends TokenKind(",")
  case Colon extends TokenKind(":")
  case ColonColon extends TokenKind("::")
  case Semicolon extends TokenKind(";")
  case Multiplication extends TokenKind("*")
  case Division extends TokenKind("/")
  case Plus extends TokenKind("+")
  case Minus extends TokenKind("-")
  case Arrow extends TokenKind("->")
  case Modulo extends TokenKind("%")
  case Equal extends TokenKind("=")
  case EqualEqual extends TokenKind("==")
  case Not extends TokenKind("!")
  case NotEqual extends TokenKind("!=")
  case Greater extends TokenKind(">")
  case GreaterEqual extends TokenKind(">=")
  case Less extends TokenKind("<")
  case LessEqual extends TokenKind("<=")
  case Apostrophe extends TokenKind("'")
  case Quote extends TokenKind("\"")
  case Amp extends TokenKind("&")
  case AmpAmp extends TokenKind("&&")
  case Pipe extends TokenKind("|")
  case PipePipe extends TokenKind("||")
  case Caret extends TokenKind("^")
  case LeftParen extends TokenKind("(")
  case RightParen extends TokenKind(")")
  case LeftBracket extends TokenKind("[")
  case LeftBracketPipe extends TokenKind("[|")
  case RightBracket extends TokenKind("]")
  case RightBracketPipe extends TokenKind("|]")
  case LeftBrace extends TokenKind("{")
  case LeftBracePipe extends TokenKind("{|")
  case RightBrace extends TokenKind("}")
  case RightBracePipe extends TokenKind("|}")
  case Dot extends TokenKind(".")
  case DotDot extends TokenKind("..")
  case At extends TokenKind("@")
  case Question extends TokenKind("?")
  case GenericLiteral(placeholder: String) extends TokenKind(s"'$placeholder")
  case IntLiteral(value: Int) extends TokenKind(value.toString)
  case DoubleLiteral(value: Double) extends TokenKind(value.toString)
  case StringLiteral(value: String) extends TokenKind(value)
  case BoolLiteral(value: Boolean) extends TokenKind(value.toString)
  case CharLiteral(value: Char) extends TokenKind(value.toString)
  case Identifier(name: String) extends TokenKind(name)
end TokenKind

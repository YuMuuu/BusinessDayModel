package businesscalendarparser

import scala.util.parsing.combinator._

/**
 * 元の評価規則
binop    ::= '+'
           | '-'
castop   ::= '_'
           | '^'
cal      ::= 'jp'
           | 'us'
           | 'c'
           | 'jp & us'
           | 'jp | us'
expr     ::= ( ( "T" | expr ) cast_op ) cal | ( "(" expr ")" ) | expr binop num
  */

/**
  * 元のEBNFをLL(1)になるように修正した評価規則
  * この規則だと"(" "T" cast_op cal ")" は評価できない。ex: "(2020/12/26^jp)"
  * jp & us -> jp&us,  jp | us -> jp|us としている。記事の最後の例ではスペースがなかった
binop    ::= '+'
           | '-'
castop   ::= '_'
           | '^'
cal      ::= 'jp'
           | 'us'
           | 'c'
           | 'jp&us'
           | 'jp|us'
expr     ::= term cast_op cal [ binop num ]
term     ::= "(" expr ")" | "T"
  */ 

object BusinessCalendarParser extends JavaTokenParsers with RegexParsers {
  private def binop = "+" | "-"
  private def castop = "_" | "^"
  private def cal: Parser[String] = "jp&us" | "jp|us" | "jp" | "us" | "c"
  private def integer = wholeNumber
  private def T: Parser[String] = """[0-9]{4}/[0-9]{2}/[0-9]{2}""".r ^^ { _.toString }
  private def expr: Parser[Any] = term ~! castop ~! cal ~! (binop ~! integer).?
  private def term: Parser[Any] = ("(" ~! expr ~! ")") | T

  def apply(input: String): Either[String, Any] = parseAll(expr, input) match {
    case Success(c, n) => Right(c)
    case Failure(e, _) => Left("FAILURE: " + e.toString)
    case Error(e, _) => Left("ERROR: " + e.toString _)
  }
}

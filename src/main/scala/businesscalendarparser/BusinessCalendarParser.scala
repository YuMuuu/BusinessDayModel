package businesscalendarparser

import scala.util.parsing.combinator._
import scala.util.Try
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.time.format.ResolverStyle

/**
 * 元の評価規則
 * binop    ::= '+'
 *            | '-'
 * castop   ::= '_'
 *            | '^'
 * cal      ::= 'jp'
 *            | 'us'
 *            | 'c'
 *            | 'jp & us'
 *            | 'jp | us'
 * expr     ::= ( ( "T" | expr ) cast_op ) cal | ( "(" expr ")" ) | expr binop num
  */

/**
  * 元のEBNFをLL(1)になるように修正した評価規則
  * この規則だと"(" "T" cast_op cal ")" は評価できない。ex: "(2020/12/26^jp)"
  * またjp & us -> jp&us,  jp | us -> jp|us としている。記事の最後の例ではスペースがなかった
  * binop    ::= '+'
  *            | '-'
  * castop   ::= '_'
  *            | '^'
  * cal      ::= 'jp'
  *            | 'us'
  *            | 'c'
  *            | 'jp&us'
  *            | 'jp|us'
  * expr     ::= term cast_op cal [ binop num ]
  * term     ::= "(" expr ")" | "T"
  */

object Token {
  sealed trait BinOp
  case object Plus extends BinOp
  case object Minus extends BinOp

  sealed trait CastOp
  case object Hat extends CastOp
  case object UnderBar extends CastOp

  sealed trait Cal
  case object JP extends Cal
  case object EN extends Cal
  case object Center extends Cal
  case object JPAndEN extends Cal
  case object JPOrEN extends Cal

  case class Num(int: Int)

  sealed trait Expr
  case class BusinessDayCalendar(
      expr: Expr,
      castOp: CastOp,
      cal: Cal,
      maybeBinOp: Option[BinOp],
      maybeInt: Option[Num]
  ) extends Expr
  
  case class Calendar(localDate: LocalDate) extends Expr
  object Calendar {
    // MEMO: このメソッド自体はjava.time.format.DateTimeParseExceptionを起こす可能性があるが、前段でcanCreateCalendarInstanceを利用して不正な値が入ってこないようにしている
    // インスタンスの生成に成功したらfilter通ってmapするcollect的な関数はないのかな？
    def fromString(date: String): Calendar = Calendar(
      LocalDate.parse(
        date,
        DateTimeFormatter
          .ofPattern("uuuu/MM/dd")
          .withResolverStyle(ResolverStyle.STRICT)
      )
    )

    def canCreateCalendarInstance(date: String): Boolean = Try(
      LocalDate.parse(
        date,
        DateTimeFormatter
          .ofPattern("uuuu/MM/dd")
          .withResolverStyle(ResolverStyle.STRICT)
      )
    ).isSuccess
  }
}

import businesscalendarparser.Token._
object BusinessCalendarParser extends JavaTokenParsers with RegexParsers {
  private def binop: Parser[BinOp] = ("+" | "-") ^^ {
    case "+" => Plus
    case "-" => Minus
  }

  private def castop: Parser[CastOp] = ("_" | "^") ^^ {
    case "_" => UnderBar
    case "^" => Hat
  }

  private def cal: Parser[Cal] = ("jp&us" | "jp|us" | "jp" | "us" | "c") ^^ {
    case "jp&us" => JPAndEN
    case "jp|us" => JPOrEN
    case "jp"    => JP
    case "us"    => EN
    case "c"     => Center
  }

  private def integer = wholeNumber ^^ { s => Num(s.toInt) }

  private def T: Parser[Calendar] =
    """[0-9]{4}/(0[1-9]|1[0-2])/(0[1-9]|[12][0-9]|3[01])""".r.filter(
      Calendar.canCreateCalendarInstance
    ) ^^ Calendar.fromString

  private def expr: Parser[BusinessDayCalendar] =
    term ~! castop ~! cal ~! (binop ~! integer).? ^^ {
      case (t: Expr) ~ (cas: CastOp) ~ (cl: Cal) ~ Some(
            (b: BinOp) ~ (i: Num)
          ) =>
        BusinessDayCalendar(t, cas, cl, Some(b), Some(i))
      case (t: Expr) ~ (cas: CastOp) ~ (cl: Cal) ~ None =>
        BusinessDayCalendar(t, cas, cl, None, None)
    }

  private def term: Parser[Expr] = ("(" ~>! expr <~! ")") | T

  def apply(input: String): Either[String, BusinessDayCalendar] =
    parseAll(expr, input) match {
      case Success(c, n) => Right(c)
      case Failure(e, _) => Left("FAILURE: " + e.toString)
      case Error(e, _)   => Left("ERROR: " + e.toString)
    }
}

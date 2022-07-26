package businesscalendarparser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.EitherValues
import businesscalendarparser._
import businesscalendarparser.Token._
import org.scalatest.matchers.should.Matchers
import java.time.LocalDate

class BusinessCalendarParserSpec extends AnyFlatSpec with EitherValues with Matchers {
  "cal eval rules" should "2020/12/26^jp" in  {
    val bcp= BusinessCalendarParser("2020/12/26^jp")
    bcp shouldBe a [Right[_, _]]
    bcp.right.get shouldBe BusinessDayCalendar(Calendar(LocalDate.of(2020, 12, 26)), Hat, JP, None, None)
  }
  
  it should "2020/12/26^us" in {
    val bcp= BusinessCalendarParser("2020/12/26^us")
    bcp shouldBe a [Right[_, _]]
    bcp.right.get shouldBe BusinessDayCalendar(Calendar(LocalDate.of(2020, 12, 26)), Hat, EN, None, None)
  }

  it should "2020/12/26^c" in {
    val bcp= BusinessCalendarParser("2020/12/26^c")
    bcp shouldBe a [Right[_, _]]
    bcp.right.get shouldBe BusinessDayCalendar(Calendar(LocalDate.of(2020, 12, 26)), Hat, Center, None, None)
  }

  it should "2020/12/26^jp|us" in {
    val bcp= BusinessCalendarParser("2020/12/26^jp|us")
    bcp shouldBe a [Right[_, _]]
    bcp.right.get shouldBe BusinessDayCalendar(Calendar(LocalDate.of(2020, 12, 26)), Hat, JPOrEN, None, None)
  }

  it should "2020/12/26^jp&us" in {
    val bcp= BusinessCalendarParser("2020/12/26^jp&us")
    bcp shouldBe a [Right[_, _]]
    bcp.right.get shouldBe BusinessDayCalendar(Calendar(LocalDate.of(2020, 12, 26)), Hat, JPAndEN, None, None)
  }

  "castop eval rules" should "2020/12/26^jp" in {
    val bcp= BusinessCalendarParser("2020/12/26^jp")
    bcp shouldBe a [Right[_, _]]
    bcp.right.get shouldBe BusinessDayCalendar(Calendar(LocalDate.of(2020, 12, 26)), Hat, JP, None, None)
  }

  it should "2020/12/26_jp" in {
    val bcp= BusinessCalendarParser("2020/12/26_jp")
    bcp shouldBe a [Right[_, _]]
    bcp.right.get shouldBe BusinessDayCalendar(Calendar(LocalDate.of(2020, 12, 26)), UnderBar, JP, None, None)
  }

  "binop eval rules" should "2020/12/26^jp+1" in {
      val bcp = BusinessCalendarParser("2020/12/26^jp+1")
      bcp shouldBe a [Right[_, _]]
      bcp.right.get shouldBe BusinessDayCalendar(Calendar(LocalDate.of(2020, 12, 26)), Hat, JP, Some(Plus), Some(Num(1)))
  }

  it should "2020/12/26^jp+10" in {
      val bcp = BusinessCalendarParser("2020/12/26^jp+10")
      bcp shouldBe a [Right[_, _]]
      bcp.right.get shouldBe BusinessDayCalendar(Calendar(LocalDate.of(2020, 12, 26)), Hat, JP, Some(Plus), Some(Num(10)))
  }

  it should "2020/12/26^jp-1" in {
      val bcp = BusinessCalendarParser("2020/12/26^jp-1")
      bcp shouldBe a [Right[_, _]]
      bcp.right.get shouldBe BusinessDayCalendar(Calendar(LocalDate.of(2020, 12, 26)), Hat, JP, Some(Minus), Some(Num(1)))
  }

  it should "2020/12/26^jp-10" in {
      val bcp = BusinessCalendarParser("2020/12/26^jp-10")
      bcp shouldBe a [Right[_, _]]
      bcp.right.get shouldBe BusinessDayCalendar(Calendar(LocalDate.of(2020, 12, 26)), Hat, JP, Some(Minus), Some(Num(10)))
  }

  "bracket eval rules" should "(2020/12/26_c)^jp" in {
      val bcp = BusinessCalendarParser("(2020/12/26_c)^jp")
      bcp shouldBe a [Right[_, _]]
      bcp.right.get shouldBe BusinessDayCalendar(BusinessDayCalendar(Calendar(LocalDate.of(2020, 12, 26)), UnderBar, Center, None, None), Hat, JP, None, None)
  }

  it should "(2020/12/26_c+1)^jp+1" in {
      val bcp = BusinessCalendarParser("(2020/12/26_c+1)^jp+1")
      bcp shouldBe a [Right[_, _]]
      bcp.right.get shouldBe BusinessDayCalendar(BusinessDayCalendar(Calendar(LocalDate.of(2020, 12, 26)), UnderBar, Center, Some(Plus), Some(Num(1))), Hat, JP, Some(Plus), Some(Num(1)))
  }

  it should "((2020/12/26_c+1)^jp+1)_us-1" in {
      val bcp = BusinessCalendarParser("((2020/12/26_c+1)^jp+1)_us-1")
      bcp shouldBe a [Right[_, _]]
      bcp.right.get shouldBe BusinessDayCalendar(BusinessDayCalendar(BusinessDayCalendar(Calendar(LocalDate.of(2020, 12, 26)), UnderBar, Center, Some(Plus), Some(Num(1))), Hat, JP, Some(Plus), Some(Num(1))), UnderBar, EN, Some(Minus), Some(Num(1)))
  }  

  // "(" "T" cast_op cal ")" は評価できない
  it should "left (2020/12/26^jp)" in {
      val bcp = BusinessCalendarParser("(2020/12/26^jp)")
      bcp shouldBe a [Left[_, _]]
      bcp.left.get shouldBe "ERROR: '^' expected but end of source found" 
  }

  //不正な日付はパースが失敗する
  "Calendar" should "left 2020/02/30^jp" in {
      val bcp = BusinessCalendarParser("2020/02/30^jp")
      bcp shouldBe a [Left[_, _]] 
      bcp.left.get shouldBe "FAILURE: Input doesn't match filter: 2020/02/30"
  }

  it should "left ((2020/02/30_c+1)^jp+1)_us-1" in {
      val bcp = BusinessCalendarParser("((2020/02/30_c+1)^jp+1)_us-1")
      bcp shouldBe a [Left[_, _]] 
      println(bcp)
      //再帰的構造のときにはERRORになる？
      bcp.left.get shouldBe "ERROR: Input doesn't match filter: 2020/02/30"
  }
}

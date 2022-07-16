package businesscalendarparser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.EitherValues
import businesscalendarparser._
import org.scalatest.matchers.should.Matchers

class BusinessCalendarParserSpec extends AnyFlatSpec with EitherValues with Matchers {
  "cal eval rules" should "2020/12/26^jp" in  {
    val bcp= BusinessCalendarParser("2020/12/26^jp")
    bcp shouldBe a [Right[_, _]]
    assert(bcp.isRight)
  }
  
  it should "2020/12/26^us" in {
    val bcp= BusinessCalendarParser("2020/12/26^us")
    bcp shouldBe a [Right[_, _]]
    assert(bcp.isRight)
  }

  it should "2020/12/26^c" in {
    val bcp= BusinessCalendarParser("2020/12/26^c")
    bcp shouldBe a [Right[_, _]]
    assert(bcp.isRight)
  }

  it should "2020/12/26^jp|us" in {
    val bcp= BusinessCalendarParser("2020/12/26^jp|us")
    bcp shouldBe a [Right[_, _]]
    assert(bcp.isRight)
  }

  it should "2020/12/26^jp&us" in {
    val bcp= BusinessCalendarParser("2020/12/26^jp&us")
    bcp shouldBe a [Right[_, _]]
    assert(bcp.isRight)
  }

  "castop eval rules" should "2020/12/26^jp" in {
    val bcp= BusinessCalendarParser("2020/12/26^jp")
    bcp shouldBe a [Right[_, _]]
    assert(bcp.isRight)
  }

  it should "2020/12/26_jp" in {
    val bcp= BusinessCalendarParser("2020/12/26_jp")
    bcp shouldBe a [Right[_, _]]
    assert(bcp.isRight)
  }

  "binop eval rules" should "2020/12/26^jp+1" in {
      val bcp = BusinessCalendarParser("2020/12/26^jp+1")
      bcp shouldBe a [Right[_, _]]
      assert(bcp.isRight)
  }

  it should "2020/12/26^jp+10" in {
      val bcp = BusinessCalendarParser("2020/12/26^jp+10")
      bcp shouldBe a [Right[_, _]]
      assert(bcp.isRight)
  }

  it should "2020/12/26^jp-1" in {
      val bcp = BusinessCalendarParser("2020/12/26^jp-1")
      bcp shouldBe a [Right[_, _]]
      assert(bcp.isRight)
  }

  it should "2020/12/26^jp-10" in {
      val bcp = BusinessCalendarParser("2020/12/26^jp-10")
      bcp shouldBe a [Right[_, _]]
      assert(bcp.isRight)
  }

"bracket eval rules" should "(2020/12/26_c)^jp" in {
      val bcp = BusinessCalendarParser("(2020/12/26_c)^jp")
      bcp shouldBe a [Right[_, _]]
      assert(bcp.isRight)
  }

  it should "(2020/12/26_c+1)^jp+1" in {
      val bcp = BusinessCalendarParser("(2020/12/26_c+1)^jp+1")
      bcp shouldBe a [Right[_, _]]
      assert(bcp.isRight)
  }

  it should "((2020/12/26_c+1)^jp+1)_us-1" in {
      val bcp = BusinessCalendarParser("((2020/12/26_c+1)^jp+1)_us-1")
      bcp shouldBe a [Right[_, _]]
      assert(bcp.isRight)
  }  

 // "(" "T" cast_op cal ")" は評価できない
  it should "left (2020/12/26^jp)" in {
      val bcp = BusinessCalendarParser("(2020/12/26^jp)")
      bcp shouldBe a [Left[_, _]]
      assert(bcp.isLeft)
  }
}

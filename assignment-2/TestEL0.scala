//-------------------------------------------------------------------------
// Supporting code for CS558 Programming Languages. 
// Dept of Computer Science, Portland State University
// Original version: A. Tolmach; modified: J. Li (8/2018)
//-------------------------------------------------------------------------

// Testing EL0 parser and printer
//
import org.scalatest.FunSuite
import EL0._

class TestEL0 extends FunSuite {
  
  test("parse rem") {
    assert(Rem(Num(11), Num(6)) == parse("(% 11 6)"))
  }
  
  test("parse sub") {
    assert(Sub(Num(11), Num(6)) == parse("(- 11 6)"))
  }
  
  test("parse add, mul, div") {
    assert(Add(Mul(Div(Num(3), Num(4)), Num(5)), Num(6))
           == parse("(+ (* (/ 3 4) 5) 6)"))
  }
  
  // ParseException for string consisting of invalid arguments (3
  // args, 1 arg, no args, empty)
  test("parse exception for s-expression with 3 arguments") {
    intercept[ParseException] { (parse("(+ 1 2 3)")) }
  }
  
  test("parse exception for s-expression with 1 argument") {
    intercept[ParseException] { (parse("(+ 1)")) }
  }
  
  test("parse exception for s-expression with no argument") {
    intercept[ParseException] { (parse("(+)")) }
  }
  
  test("parse exception for empty s-expression") {
    intercept[ParseException] { (parse("()")) }
  }
  
  // test pretty printer

  test("print rem") {
    assert(Rem(Num(11), Num(6)).toString() == "(% 11 6)")
  }

  test("print sub") {
    assert(Sub(Num(11), Num(6)).toString() == "(- 11 6)")
  }
  
  test("print add, mul, div") {
    assert(Add(Mul(Div(Num(11), Num(5)), Num(3)), Num(6)).toString()
           == "(+ (* (/ 11 5) 3) 6)")
  }
  
  // test reader, parser and pretty printer working together

  test("parse(print(parse(s))) == parse(s))") {
    val expr = parse("(+ (* (/ 11 5) 3) 6)")
    assert(parse(expr.toString()) == expr)
  }

}

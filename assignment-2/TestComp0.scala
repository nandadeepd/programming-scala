/*
* @Author: Nandadeep Davuluru
* @Date:   2018-10-07 16:28:50
* @Last Modified by:   nandadeepd
* @Last Modified time: 2018-10-09 17:23:02
*/

import org.scalatest.FunSuite
import Comp0._
import Machine0._

class TestComp0 extends FunSuite {

	test("correctly interpret an expression involving add, mul, div") {
    assertResult(6)(process("(+ (* (/ 3 4) 5) 6)"))
  }
	
  test("correctly interpret an expression involving sub") {
    assertResult(1)(process("(- (- (- 10 3) 3) 3)"))
  }
	
  // test non-zero inputs for mod, 0 in each arg position, negative
  // args in each arg pos
  test("correctly interpret an expression involving rem"){
    assertResult(0)(process("(% 10 2)"))
  } 

  // Exceptions are run time so not testing them in compiler. 
  
  test("correct behavior for div by 0") {
    intercept[ExecException]{process("(/ 10 0)")}
  }
  
  test("correct behavior for rem by 0"){
    intercept[ExecException]{process("(% 10 0)")}
  } 
  
  test("correct behavior for rem positive by negative"){
    assertResult(1)(process("(% 10 -3)"))
  } 
  
  test("correct behavior for rem negative by positive") {
    assertResult(-1)(process("(% -10 3)"))
  }
  
  test("correct behavior for rem negative by negative") {
    assertResult(-1)(process("(% -10 -3)"))
  }
	
  test ("correctly interpret a complicated arithmetic expression") {
    assertResult(5)(process("(* 1 (+ 2 (- 3 (/ 4 (% 6 7)))))"))
  }


}
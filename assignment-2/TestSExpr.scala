/*
* @Author: Nandadeep Davuluru
* @Date:   2018-10-08 19:04:21
* @Last Modified by:   nandadeepd
* @Last Modified time: 2018-10-08 19:04:28
*/

//-------------------------------------------------------------------------
// Supporting code for CS558 Programming Languages. 
// Dept of Computer Science, Portland State University
// Original version: A. Tolmach; modified: J. Li (8/2018)
//-------------------------------------------------------------------------

// Testing SExpr reader and printer
//
import org.scalatest.FunSuite
import SExprLibrary._

class TestSExpr extends FunSuite {
  
  test("reading a number 123") {
    assert(SNum(123) == SExprReader.read("123"))
  }
  
  test("reading a number 456") {
    assertResult(SNum(456))(SExprReader.read("456"))
  }
  
  test("printing a number 789") {
    assert("789" == SExprPrinter.print(SNum(789)))
  }


  // additional constructor test cases 
  test("testing SSym, SString") {

  	
  	assert(SExprReader.read("\"Xyz\"") == SString("Xyz"))
  	assert(SExprReader.read("X") == SSym("X"))
  }

  test("Testing SList") {
  	assert(SList(List(SNum(123), SNum(456))) == SList(List(SExprReader.read("123"), SExprReader.read("456"))))
  }

}

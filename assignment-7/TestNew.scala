// Nandadep Davuluru, davuluru@pdx.edu
//-------------------------------------------------------------------------

// Testing new EL3 programs
//
import org.scalatest.FunSuite
import Interp3._

class TestNew extends FunSuite {

  // Put your program code inside the triple quotes
  // multi-arg functions
  val f1Code = """(let f (fun x (fun y (- x y))) (@ (@ f 4) 2))"""
  val f2Code = """(let f (fun x (fun y (fun z (- (+ x y) z)))) (@ (@ (@ f 2) 3) 4))"""

  // fibonacci (regular)
  val fibCode = """(letRec fib (fun n (if (<= n 1) 1 (+ n (@ fib (- n 1))))) (@ fib 10))"""

  // fibonacci (tail-recursive)
  val fibtCode = """(letRec helper (fun n (fun a (fun b (if (<= n 2) a (@ (@ (@ helper (- n 1)) (+ a b)) a))))) (@ ( @ (@ helper 10) 1) 1))"""

  // fibonacci (cps)
  //val fibcCode = """()"""

  // Test the programs with heap storage

  test("multi-arg functions") {
    assertResult(2) { process(f1Code,true) }
    assertResult(1) { process(f2Code,true) }
  }

  test("fibonacci (regular)") {
    assertResult(55) { process(fibCode,true) }
  }

  test("fibonacci (tail-recursive)") {
    assertResult(55) { process(fibtCode,true) }
  }

  // optional 
  //test("fibonacci (cps)") {
    //assertResult(55) { process(fibcCode,true) }
  //}
}

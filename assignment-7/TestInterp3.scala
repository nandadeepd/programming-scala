//-------------------------------------------------------------------------
// Supporting code for CS558 Programming Languages. 
// Dept of Computer Science, Portland State University
// Original version: A. Tolmach; modified: J. Li (8/2018)
//-------------------------------------------------------------------------

// Testing EL3 interpreter
//
import org.scalatest.FunSuite
import Interp3._

class TestInterp3 extends FunSuite {
  

  // test("letrec") {
  //   process("(letRec f (+ a 1) (@ f 3))")
  // }
  test("test let" ) {
    assertResult(3) { process("(let x 1 (let y 2 (+ x y)))") }
    assertResult(4) { process("(let x 1 (let x 2 (+ x x)))") }
    intercept[InterpException] { process("(letRec x 1 (+ x 1))") }
  }

  // Simple functions

  test("simple functions" ) {
    assertResult(6) { process("(@ (fun x (+ x 1)) 5)") }
    assertResult(6) { process("(let f (fun x (+ x 1)) (@ f 5))") }
    assertResult(26) { process("(let f (fun x (+ (* x x) 1)) (@ f 5))") }
  }

  // // Free-variables storage (stack vs. heap)

  val example1 = """(@ (@ (fun x (fun y (+ x y))) 2) 3)"""

  val example2 = """(let f (fun x (fun y x)) (@ (@ f 2) 1))"""

  val example3 = """(let f (let z 0 (let y 3 (fun x y))) (@ f 1))"""             
    
  val example4 = """(let f (let y 4 (fun x y)) (@ f 1))"""

  test("use stack storage for variables/parameters" ) {
    assertResult(6) { process(example1) }
    assertResult(1) { process(example2) }
    assertResult(1) { process(example3) }
    intercept[InterpException] { process(example4) }
  }

  test("use heap storage for variables/parameters" ) {
    assertResult(5) { process(example1,true) }
    assertResult(2) { process(example2,true) }
    assertResult(3) { process(example3,true) }
    assertResult(4) { process(example4,true) }
  }

  // Factorial function
                        // f                  b                                 e
  val facCode = """(letRec fac (fun n (if (<= n 1) 1 (* n (@ fac (- n 1))))) (@ fac 5))"""

  test("factorial function") {
   process(facCode)
  }

  // Call-by-name 

  test("call-by-name") {
    assertResult(6) { process("(@ (fun x (+ x 1)) 5)",cbn=true) }
    assertResult(6) { process("(let f (fun x (+ x 1)) (@ f 5))",cbn=true) }
    assertResult(26) { process("(let f (fun x (+ (* x x) 1)) (@ f 5))",cbn=true) }

    assertResult(5) { process(example1,true,true) }
    assertResult(2) { process(example2,true,true) }
    assertResult(3) { process(example3,true,true) }
    assertResult(4) { process(example4,true,true) }
    assertResult(120) { process(facCode,true,true) }
    assertResult(55) { process(facCode,true,true) }
  }

  test("call-by-name (lazy-eval)") {
    intercept[InterpException] { process("(let f (fun x 1) (@ f (/ 5 0)))") }
    assertResult(1) { process("(let f (fun x 1) (@ f (/ 5 0)))",true,true) }
  }

  test("call-by-name with alpha-reduction") {
    assertResult(2) { process("(@ (fun x (let x 1 (+ x 1))) 5)",true,true) }
    assertResult(3) { process("(@ (@ (fun x (fun y (+ y 1))) 5) 2)",true,true) }
    assertResult(3) { process("(let x 0 (@ (@ (fun y (fun x (+ x y))) (+ x 1)) 2))",
                              true,true) }
  }

}

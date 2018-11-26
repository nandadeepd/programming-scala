//-------------------------------------------------------------------------
// Supporting code for CS558 Programming Languages. 
// Dept of Computer Science, Portland State University
// Original version: A. Tolmach; modified: J. Li (8/2018)
//-------------------------------------------------------------------------

// Testing EL4 typechecker
//
import org.scalatest.FunSuite
import Check._

class TestCheck extends FunSuite {
  def assertConsoleOutput(p:String, s:String) = {
    val output = new java.io.ByteArrayOutputStream(10240)
    def assertRes = assertResult(p){
      process(s); output.flush(); output.toString
    }
    Console.withOut(output)(assertRes)
  }

  test("test arith" ) {
    assertConsoleOutput("", "(* (+ 1 2) (- 6 4))") 
    assertConsoleOutput("", "(&& (<= 5 3) (|| true (<= 2 3)))")
    intercept[TypingException]{ process("(+ true 4)") }
    intercept[TypingException]{ process("(&& 2 4)") }
    intercept[TypingException]{ process("(|| false (- 6 4))") }
    intercept[TypingException]{ process("(<= true true)") }
    intercept[TypingException]{ process("(+ x 1)") }
  }

  test("test let" ) {
    assertConsoleOutput("", "(let x intT 1 (let y intT 2 (+ x y)))") 
    assertConsoleOutput("", "(let x intT 1 (let x intT 2 (+ x x)))") 
    assertConsoleOutput("", "(let x boolT (<= 1 2) (|| x false))") 
    intercept[TypingException]{ process("(let x boolT 1 (+ x 1))") }
    intercept[TypingException]{ process("(let x boolT true (+ x 1))") }
    intercept[TypingException]{ process("(let x intT true (+ x 1))") }
    intercept[TypingException]{ process("(letRec x intT 1 (+ x 1))") }
  }

  // If 
  test("test if" ) {
    assertConsoleOutput("", "(if true 1 2)")
    assertConsoleOutput("", "(if (<= 1 2) true false)")
    intercept[TypingException]{ process("(if 1 2 3)") }
    intercept[TypingException]{ process("(if true true 1)") }
  }

  // Simple functions

  test("simple functions" ) {
    assertConsoleOutput("", "(@ (fun x intT (+ x 1)) 5)") 
    assertConsoleOutput("", "(let x (funT intT boolT) (fun x intT true) 1)") 
    assertConsoleOutput("", "(let f (funT intT intT) (fun x intT (+ x 1)) (@ f 5))") 
    assertConsoleOutput("", "(let f (funT intT intT) (fun x intT (+ (* x x) 1)) (@ f 5))") 
  }

  test("function errors" ) {
    intercept[TypingException]{ process("(@ (fun x intT (+ x 1)) true)") }
    intercept[TypingException]{ process("(let x (funT intT boolT) 5 5)") }
    intercept[TypingException]{ process("(let x (funT intT intT) (fun x boolT true) (@ x 1))") }
    intercept[TypingException]{ process("(letRec x (funT intT boolT) 5 5)") }
  }

  // Free-variables

  val example1 = """(@ (@ (fun x intT (fun y intT (+ x y))) 
                          2) 
                       3)"""

  val example2 = """(let f (funT intT (funT intT intT)) 
                         (fun x intT (fun y intT x))  
                         (@ (@ f 2) 1))"""

  val example3 = """(let f (funT intT intT) 
                         (let z intT 0                  
                           (let y intT 3 
                             (fun x intT y)))   
                         (@ f 1))"""             
    
  val example4 = """(let f (funT intT intT) 
                         (let y intT 4 (fun x intT y))
                         (@ f 1))"""

  test("free variables" ) {
    assertConsoleOutput("", example1)
    //assertConsoleOutput("", example2)
    //assertConsoleOutput("", example3)
    //assertConsoleOutput("", example4)
  }

  // Factorial functions

  val facCode = """(letRec fac (funT intT intT) 
                           (fun n intT (if (<= n 1)
                                  1 
                                  (* n (@ fac (- n 1)))))
                           (@ fac 5))"""

  test("factorial function") {
    assertConsoleOutput("", facCode)
  }

}

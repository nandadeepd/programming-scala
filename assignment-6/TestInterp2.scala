//-------------------------------------------------------------------------
// Supporting code for CS558 Programming Languages. 
// Dept of Computer Science, Portland State University
// Original version: A. Tolmach; modified: J. Li (8/2018)
//-------------------------------------------------------------------------

// Testing EL2 interpreter
//
import org.scalatest.FunSuite
import Interp2._

class TestInterp2 extends FunSuite {

  // some shorthands
  def run(s:String) = process(s)
  
  def expr(s:String) : String = "(() " + s + ")"

  def runExpr(s:String) : Int = run(expr(s))
  
  // compare expected and actual console output (could be multi-lines)
  def assertConsoleOutput(p:String, s:String) = {
    val output = new java.io.ByteArrayOutputStream(10240)
    def assertRes = assertResult(p){run(s);output.flush();output.toString}
    Console.withOut(output)(assertRes)
  }
  
  def assertExprOutput(p:String, s:String) = assertConsoleOutput(p, expr(s))
  
  // Basic tests
 
  test("arithmetic") {
    assertResult(0)  { runExpr("0") }
    assertResult(-1) { runExpr("-1") }
    assertResult(12) { runExpr("12") }
    assertResult(0)  { runExpr("(* 0 7)") }
    assertResult(-3) { runExpr("(- 0 3)") }
    assertResult(12) { runExpr("(* 6 2)") }
    assertResult(0)  { runExpr("(<= 1 0)") }
    assertResult(1)  { runExpr("(<= 0 0)") }
    assertResult(1)  { runExpr("(<= 0 1)") }
  }
  
  test("if") {
    assertResult(0) { runExpr("(if 1 0 1)") }
    assertResult(0) { runExpr("(if 0 1 0)") }
    assertResult(0) { runExpr("(if 2 0 1)") }
    assertExprOutput("3\n", "(if 0 (write 1) (write 3))")
    assertExprOutput("0\n", "(if 1 (write 0) (write 1))")
    intercept[InterpException] { runExpr("(if (pair 1 2) 0 1)") }
  }
  
  test("write") {
    assertExprOutput("3\n", "(write 3)")
    assertExprOutput("", "0")
    assertExprOutput("2\n1\n", "(seq (write 2) (write 1))")
  }
  
  test("simple assignments involving globals") {
    assertResult(0) { run("(((s 0)) (:= s s))") }
    assertResult(1) { run("(((s 0)) (:= s 1))") }
    assertResult(1) { run("(((s 0) (t 1)) (:= s t))") }
  }
  
  // Tests added (rather than adapted) for this assignment are below.

  // Pair:
  test("test some simple pairs") {
    intercept[InterpException] { runExpr("(pair 1 2)") }
    assertExprOutput("(1.2)\n", "(seq (write (pair 1 2)) 0)")
    assertExprOutput("(1.(2.0))\n", "(seq (write (pair 1 (pair 2 0))) 0)")
    assertResult(3) { runExpr("(fst (pair 3 2))") }
    assertResult(6) { runExpr("(snd (pair 2 6))") }
    assertResult(-4) { run("(((a (pair 3 7))) (- (fst a) (snd a)))") }
    assertResult(6) { runExpr("(snd (snd (pair 2 (pair 2 6))))") }
    assertResult(1) { runExpr("(isPair (pair 2 6))") }
    assertResult(0) { runExpr("(isPair 3)") }
    assertExprOutput("1\n2\n", "(seq (pair (write 1) (write 2)) 0)")
    intercept[InterpException] { runExpr("(fst 0)") }
    intercept[InterpException] { runExpr("(snd 2)") }
    intercept[InterpException] { runExpr("(+ 0 (pair 1 2))") }
    intercept[InterpException] { runExpr("(<= (pair 1 2) (pair 3 4))") }    
  }
  
  // Let:
  test ("constant let body returns correctly") {
    assertResult(3) { runExpr("(let x 1 3)") }
  }
  
  test("let variable shows up in environment") {
    assertResult(7) { runExpr("(let x 1 (seq x 7))") }
  }

  test("let variable evaluated correctly") {
    assertResult(13) { runExpr("(let x 13 x)") }
  }
  
  test("let variable shadows global variable") {
    assertResult(5) { run("(((a 3)) (let a 5 a))") }
  }
  
  test("let variable shadows outer-scoped let variables") {
    assertResult(4) { runExpr("(let x 3 (let x 4 x))") }
    assertResult(4) { runExpr("(let x 3 (let x (+ 1 x) x))") }
  }
  
  test("let order-of-evaluation: e is evaluated exactly once, before b") {
    assertExprOutput("1\n2\n", 
      "(let x (write 1) (seq (seq (seq (seq (seq x x) x) (write 2)) x) x))")
  }

  // setFst/setSnd:
  test("setFst/setSnd order of evaluation") {
    assertExprOutput("(1.2)\n3\n", "(seq (setFst (write (pair 1 2)) (write 3)) 0)")
    assertExprOutput("(1.2)\n3\n", "(seq (setSnd (write (pair 1 2)) (write 3)) 0)")
  }
  
  test("setFst/setSnd runtime error if first arg not pair") {
    intercept[InterpException] { runExpr("(setFst 0 0)") }
    intercept[InterpException] { runExpr("(setSnd 0 0)") }
  }

  test("setFst updates appropriate component") {
    assertResult(3) { run("(((a (pair 1 2))) (seq (setFst a 3) (fst a)))") }
  }

  test("setSnd updates appropriate component") {
    assertResult(3) { run("(((a (pair 1 2))) (seq (setSnd a 3) (snd a)))") }
  }

  test("setFst/setSnd do not touch the irrelevant component and return the modified pair value") {
    assertResult(3) { run("(((a (pair 0 0))) (seq (setFst a 3) (- (fst a) (snd a))))") }
    assertResult(-3) { run("(((a (pair 0 0))) (seq (setSnd a 3) (- (fst a) (snd a))))") }
    assertResult(3) { runExpr("(fst (setSnd (pair 3 1) 7))") }
    assertResult(3) { runExpr("(snd (setFst (pair 1 3) 7))") }
    assertResult(7) { runExpr("(fst (setFst (pair 1 3) 7))") }
    assertResult(7) { runExpr("(snd (setSnd (pair 3 1) 7))") }
  }
 
  val exampleProgram = 
    """(((a 10))
         (let a 1
            (let b a
               (seq 
                  (let a 100
                     (seq
                        (:= b (+ a b))
                        (:= a 0)))
                  (+ a b)))))"""

  test("example program for let") { assertResult(102) { run(exampleProgram) } }
  
  // Eq:
  test("Eq tests") {
    assertResult(1) { runExpr("(= (+ 2 4) (* 2 3))") } 
    assertResult(0) { runExpr("(= (pair 2 4) (pair 2 4))") } 
    assertResult(1) { run("(((x (pair 1 2))) (let y x (= x y)))") } 
    intercept[InterpException] { runExpr("(= 5 (pair 1 2))") }
  }

  // Deq:
  test("Deq tests") {
    assertResult(1) { runExpr("(== (+ 2 4) (* 2 3))") } 
    assertResult(1) { runExpr("(== (pair 2 4) (pair 2 4))") } 
    assertResult(1) { runExpr("(== (pair 2 (pair 1 3)) (pair 2 (pair 1 3)))") } 
    assertResult(1) { run("(((x (pair 2 (pair 1 3)))) (== x (pair 2 (pair 1 3))))") } 
    assertResult(0) { run("(((x (pair 1 2)) (y (pair 1 3))) (== x y))") } 
    intercept[InterpException] { runExpr("(== 5 (pair 1 2))") }
  }
}

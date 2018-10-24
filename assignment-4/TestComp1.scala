//-------------------------------------------------------------------------
// Supporting code for CS558 Programming Languages. 
// Dept of Computer Science, Portland State University
// Original version: A. Tolmach; modified: J. Li (8/2018)
//-------------------------------------------------------------------------

// Testing EL1 compiler
//
import org.scalatest.FunSuite
import Machine1._
import Comp1._

class NullStream extends java.io.OutputStream { def write(x:Int) = {} }

class TestComp1 extends FunSuite {
  // supress console output
  def runSilent(s:String) = Console.withOut(new NullStream)(process(s))

  // compare expected and actual console output (could be multi-lines)
  def assertConsoleOutput(p:String, s:String) = {
    val output = new java.io.ByteArrayOutputStream(10240)
    def assertRes = assertResult(p){process(s);output.flush();output.toString}
    Console.withOut(output)(assertRes)
  }

  // Programs used in the tests

  val easyProgram1 = "(for i 0 10 (write i))"
  val easyProgram2 = "(for i 0 9 (for j 0 9 (write (+ j (* 10 i)))))"

  // this should print 2 8 and evaluate to 0
  val trickyProgram1 = 
    "(for i 0 (seq (:= i (+ i 2)) 10) (seq (write i) (:= i (+ i 3))))"

  val trickyProgram2 = 
    "(seq (for i 3 (seq (:= i (+ s 8)) 44) (seq (write i) (:= s (+ i 7)))) i)"

  // store side effects of for loop modifying variables from outside
  val sumProgram = "(seq (for i 0 10 (seq (write s) (:= s (+ s i)))) s)"

  // find all primes between 2 to 50
  val primesProgram = 
    """(seq
         (:= n 2)
         (while (<= n 50)
           (seq (seq (seq (seq
             (:= prime 1)
             (:= d 2))
             (while (<= d (- n 1))
               (seq (seq
                 (:= m (* d (/ n d)))  
                 (if (<= n m)
                     (:= prime 0)
                     (skip)))
                 (:= d (+ d 1)))))
             (if prime (write n) (skip)))
             (:= n (+ n 1)))))"""

  val primesResult = 
    "2\n3\n5\n7\n11\n13\n17\n19\n23\n29\n31\n37\n41\n43\n47\n"

  test("variable implicit init") {
    assertResult(0){process("x")}
    assertResult(0){process("foo")}
    assertResult(0){process("bar1")}
  }

  test("int literals") {
    assertResult(0){process("0")}
    assertResult(-1){process("-1")}
    assertResult(12){process("12")}
  }
  
  test("arithmetic") {
    assertResult(0){process("(* 0 7)")}
    assertResult(-3){process("(- 0 3)")}
    assertResult(12){process("(* 6 2)")}
    assertResult(12){process("(/ 24 2)")}
    intercept[ExecException]{process("(/ 7 0)")}
  }

  test("le comparison") {
    assertResult(0){process("(<= 1 0)")}
    assertResult(1){process("(<= 0 0)")}
    assertResult(1){process("(<= 0 1)")}
  }

  test("simple assignments") {
    assertResult(0){process("(:= s s)")}
    assertResult(1){process("(:= s 1)")}
  }
  
  test("while returns 0") {
    assertResult(0){process("(while 0 0)")}
    assertResult(0){process("(seq (:= s 3) (while s (:= s (- s 1))))")}
  }
  
  test("if result") {
    assertResult(0){process("(if 1 0 1)")}
    assertResult(0){process("(if 0 1 0)")}
    assertResult(0){process("(if 2 0 1)")}
  }
  
  test("skip") {
    assertResult(0){process("(skip)")}
  }
  
  test("write") {
    assertConsoleOutput("3\n", "(write 3)")
    assertConsoleOutput("2\n1\n", "(seq (write 2) (write 1))")
  }

  test("if evaluates only one branch") {
    assertConsoleOutput("0\n", "(if 0 (write 1) (write 0))")
    assertConsoleOutput("0\n", "(if 1 (write 0) (write 1))")
  }
  
  test("while + assignment + write") {
    assertConsoleOutput("3\n2\n1\n",
      "(seq (:= s 3) (while s (seq (write s) (:= s (- s 1)))))")
  }

  test("Simple for loops, each returns a 0") {
    assertResult(0){runSilent(easyProgram1)}
    assertResult(0){runSilent(easyProgram2)}
  }

  test("Tricky for loops") {
    assertResult(0){runSilent(trickyProgram1)}
    assertResult(53){runSilent(trickyProgram2)}
  }
  
  test("Sum of 1 to 10") {
    assertResult(55){runSilent(sumProgram)}
  }

  test("Primes that are < 50") {
    assertConsoleOutput(primesResult, primesProgram) 
  }

}

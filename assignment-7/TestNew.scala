// Nandadep Davuluru, davuluru@pdx.edu
//-------------------------------------------------------------------------

// Testing new EL3 programs
//
import org.scalatest.FunSuite
import Interp3._

class TestNew extends FunSuite {

  // Put your program code inside the triple quotes

  // multi-arg functions
  val f1Code = """()"""
  val f2Code = """()"""

  // fibonacci (regular)
  val fibCode = """()"""

  // fibonacci (tail-recursive)
  val fibtCode = """()"""

  // fibonacci (cps)
  val fibcCode = """()"""

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

  test("fibonacci (cps)") {
    assertResult(55) { process(fibcCode,true) }
  }
}

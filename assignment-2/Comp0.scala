// Nandadeep Davuluru, davuluru@pdx.edu
//-------------------------------------------------------------------------

// EL0 Compiler
//
// Usage: linux> scala Comp0 <source file>
//
import EL0._
import Machine0._

object Comp0 {

  def compile(e: Expr): Program = e match {
    case Num(i) => Const(i)::Nil
    case Add(l, r) => compile(l) ::: compile(r) ::: Plus :: Nil
    case Sub(l, r) => compile(l) ::: compile(r) ::: Subtract :: Nil
    case Mul(l, r) => compile(l) ::: compile(r) ::: Times :: Nil
    case Div(l, r) => compile(l) ::: compile(r) ::: Divrem :: Pop :: Nil
    case Rem(l, r) => compile(l) ::: compile(r) ::: Divrem :: Swap :: Pop :: Nil
  }

  def process(s:String, debug:Int = 0): Int = {
    try {
      val e: Expr = parse(s,debug)
      val p: Program = compile(e)
      if (debug > 0) println("Compiles to:" + p)
      try {
        val r = exec(p,debug)
        println("Result:" + r)
        r 
      } catch {
        case ex: ExecException => {
	  println("Exec Error:" + ex.string) ; throw ex
	}
      }
    } catch {
      case ex: ParseException => {
	println("Parser Error:" + ex.string) ; throw ex
      }
    }
  }

  // Test driver
  import scala.io.Source
  def main(argv: Array[String]) = {
    val s = Source.fromFile(argv(0)).getLines.mkString("\n")
    val d = if (argv.length > 1) argv(1).toInt else 0
    process(s,d)
    ()
  }
}
//

// Nandadeep Davuluru, davuluru@pdx.edu
//-------------------------------------------------------------------------

// EL4 Typechecker
//
// Usage: linux> scala Check <source file>
//
import collection.immutable.Map._
import EL4._

object Check {
  case class TypingException(string: String) extends RuntimeException

  type Env = Map[String,Type]

  val emptyEnv:Env = Map[String,Type]()

  def check(e:Expr,debug:Int = 0) = {

    def checkVar(env:Env,x:String): Type =
      env.getOrElse(x, throw TypingException("undefined variable:" + x))
  
    def checkE(env:Env,e:Expr): Type = e match {
      case Num(_)   => IntTy
      case Bool(_)  => BoolTy
      case Var(x)   => checkVar(env,x)
      case Add(l,r) => // ... add code ...
      case Sub(l,r) => // ... add code ...
      case Mul(l,r) => // ... add code ...
      case Div(l,r) => // ... add code ...
      case And(l,r) => // ... add code ...
      case Or(l,r)  => // ... add code ...
      case Le(l,r)  => // ... add code ...
      case If(c,t,e) => // ... add code ...
      case Seq(e1,e2) => { checkE(env,e1); checkE(env,e2) }
      case Fun(p,t,b) => // ... add code ...
      case Apply(f,e) => // ... add code ...
      case Let(x,t,d,b) => // ... add code ...
      case LetRec(x,t,d,b) => // ... add code ...
    }
    
    checkE(emptyEnv,e)
    if (debug > 0) println("Checked")
  }

  def process(s:String, debug:Int = 0) = {
    try {
      val e: Expr = parse(s,debug)
      check(e,debug)
    } catch {
      case ex: TypingException => 
        { println("Typing error: " + ex.string); throw ex }
      case ex: ParseException => 
        { println("Parser Error:" + ex.string); throw ex }
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

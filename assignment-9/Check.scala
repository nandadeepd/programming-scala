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
      case Add(l,r) => {
        checkE(env, l) match {
          case IntTy => IntTy
          case _ => throw TypingException("not a number type")
        }
        checkE(env, r) match {
          case IntTy => IntTy
          case _ => throw TypingException("not a number type")
        }

      }
      case Sub(l,r) => {
        checkE(env, l) match {
          case IntTy => IntTy
          case _ => throw TypingException("not a number type")
          }
        checkE(env, r) match {
          case IntTy => IntTy
          case _ => throw TypingException("not a number type")
          }
      }
      case Mul(l,r) => {
        checkE(env, l) match {
          case IntTy => IntTy
          case _ => throw TypingException("not a number type")
        }
        checkE(env, r) match {
          case IntTy => IntTy
          case _ => throw TypingException("not a number type")
        }

      }
      case Div(l,r) => {
        checkE(env, l) match {
          case IntTy => IntTy
          case _ => throw TypingException("not a number type")
        }
        checkE(env, r) match {
          case IntTy => IntTy
          case _ => throw TypingException("not a number type")
        }

      }
      case And(l,r) => {
        checkE(env, l) match {
          case BoolTy => BoolTy
          case _ => throw TypingException("AND left failed")
        }
        checkE(env, r) match {
          case BoolTy => BoolTy
          case _ => throw TypingException("AND right failed")
        }

      }
      case Or(l,r)  => {
        checkE(env, l) match {
          case BoolTy => BoolTy
          case _ => throw TypingException("OR left failed")
        }
        checkE(env, r) match {
          case BoolTy => BoolTy
          case _ => throw TypingException("OR right failed")
        }
      }
      case Le(l,r)  => {
        val lType = checkE(env, l)
        lType match {
          case IntTy => {}
          case _ => throw TypingException("left has wrong type in LEQ")
        }
        val rType = checkE(env, r)
        rType match {
          case IntTy => {}
          case _ => throw TypingException("right has wrong type in LEQ")
        }
        if (lType == rType) BoolTy
        else throw TypingException("expr has wrong type in LEQ")
      }
      case If(c,t,e) => {
        
        val cType = checkE(env, c); cType match {
          case BoolTy => BoolTy
          case _ => throw TypingException("C isn't bool")
        }
        val tType = checkE(env, t)
        if (tType == checkE(env, e)) tType
        else throw TypingException("not type ty")
      }
      case Seq(e1,e2) => { checkE(env,e1); checkE(env,e2) }
      //case Fun(p,t,b) => // ... add code ...
      //case Apply(f,e) => // ... add code ...
      //case Let(x,t,d,b) => // ... add code ...
      //case LetRec(x,t,d,b) => // ... add code ...
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

// Nandadeep Davuluru
//-------------------------------------------------------------------------

// EL1 Interpreter
//
// Usage: linux> scala Interp1 <source file>
//
import EL1._

object Interp1b {
  case class InterpException(string: String) extends RuntimeException

  type Store = collection.mutable.Map[String,Int]
  
  def interp(e:Expr, debug:Int = 0): Int = {
    
    val st: Store = collection.mutable.Map[String,Int]()

    def interpE(e:Expr): Int = {
      if (debug > 1) {
        println("  expr = " + e);
        println("  store = " + st)
      }
      e match {
        case Num(n) => n
        case Var(x) => 
          st get x match {
            case Some(v) => v
            case None    => 0
          }
        case Assgn(x, e) => {
          st(x) = interpE(e)
          st(x)
        }
        
        case Add(l,r) => interpE(r) + interpE(l)
        case Sub(l, r) => {
          var right = interpE(r)
          interpE(l) - right
        }
        case Div(l, r) => {
          var ri: Int = interpE(r)
          if (ri == 0) {throw new InterpException("Divide by zero")}
          else {interpE(l)/ ri}
        }
        case Rem(l, r) => {
          var ri: Int = interpE(r)
          if (ri == 0) {throw new InterpException("Divide by zero")}
          else {interpE(l) % ri}
        }
        case Mul(l, r) => interpE(r) * interpE(l)
        case Le(e1, e2) => {
          if(interpE(e2) >= interpE(e1)) {
            1
          } else {
            0
          }
        }

        case While(c,b) => {
          if(interpE(c) != 0) {
            interpE(b)
            interpE(While(c, b))
          }
          0
        }

        case If(c,t,e) => {
          if (interpE(c) != 0) {
            val v = interpE(t)
            v
          } else {
            val v = interpE(e)
            v
          }
        }

        case Write(e) => {
          val v = interpE(e)
      	  println(v);
          v
        }
        case Seq(e1,e2) => {
          val v1 = interpE(e1)
          val v2 = interpE(e2)
          v2
        }
        case Skip() => 0
        case For(x, e1, e2, e3) => {

          st(x) = interpE(e1)
          var v2 = interpE(e2)
          var vx = st(x)
          while(vx <= v2) {

            interpE(e3)
            st(x) += 1
            v2 = interpE(e2)
            vx = st(x)
          }
          0

        }
      }
    }

    val v = interpE(e)
    if (debug > 0) println("Evaluates to: " + v)
    v
  } 
  
  def process(s:String, debug:Int = 0): Int = {
    try {
      val e: Expr = parse(s,debug)
      interp(e,debug)
    } catch {
      case ex: InterpException => 
        { println("Interp Error:" + ex.string) ; throw ex }
      case ex: ParseException => 
        { println("Parser Error:" + ex.string) ; throw ex }
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

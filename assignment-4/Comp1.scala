// Nandadeep Davuluru
//-------------------------------------------------------------------------

// EL1 Compiler
//
// Usage: linux> scala Comp1 <source file>
//
import EL1._
import Machine1._

object Comp1 {
  var nextLabel: Int = 0

  def comp(e:Expr): Program = e match {
    case Var(x) => Load(x) :: Nil
    case Num(i) => Const(i) :: Nil
    case Add(e1, e2) => comp(e1) ::: comp(e2) ::: Plus :: Nil
    case Sub(e1, e2) => comp(e1) ::: comp(e2) ::: Const(-1) :: Times :: Plus :: Nil
    case Mul(e1, e2) => comp(e1) ::: comp(e2) ::: Times :: Nil
    case Div(e1, e2) => comp(e1) ::: comp(e2) ::: Divrem :: Pop :: Nil
    case Rem(e1, e2) => comp(e1) ::: comp(e2) ::: Divrem :: Swap :: Pop :: Nil
    case Le(e1, e2)  => comp(e1) ::: comp(e2) ::: Lessequ :: Nil
    case Assgn(x, e) => comp(e) ::: Dup :: Store(x) :: Nil
    case If(c, t, e)  => {
      val a = newLabel(); val b = newLabel(); val d = newLabel();
      comp(c) ::: Branchz(a) :: Branch(b) :: Label(a) :: comp(e) ::: Branch(d) :: Label(b) :: comp(t) ::: Branch(d) :: Label(d) :: Nil
    }
    case While(c, e) => {
      val start = newLabel(); val done = newLabel();
      Label(start) :: comp(c) ::: Branchz(done) :: comp(e) ::: Pop :: Branch(start) :: Label(done) :: Const(0) :: Nil
    }
    case Write(e)   => comp(e) ::: Dup :: Print :: Nil
    case Seq(e1, e2) => comp(e1) ::: comp(e2) ::: Swap :: Pop :: Nil
    case Skip() => Const(0) :: Nil
    case For(x, e1, e2, e3) => {
      val while_label_start = newLabel(); val compute = newLabel();
      comp(e1) ::: Store(x) :: Branch(while_label_start) :: comp(e2) ::: Load(x) :: Swap :: Lessequ :: Branchz(compute) :: Branch(done) 

    }
  }

  def newLabel() = {
    val next = nextLabel
    nextLabel = nextLabel + 1
    next
  }


  def compile(e:Expr) = {
    nextLabel = 0
    comp(e)
  }

  def process(s:String, debug:Int = 0): Int = {
    try {
      val e: Expr = parse(s,debug)
      val p: Program = compile(e)
      exec(p,debug)
    } catch {
      case ex: ExecException =>
        { println("Exec Error:" + ex.string) ; throw ex }
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

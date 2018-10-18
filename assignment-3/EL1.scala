//-------------------------------------------------------------------------
// Supporting code for CS558 Programming Languages. 
// Dept of Computer Science, Portland State University
// Original version: A. Tolmach; modified: J. Li (8/2018)
//-------------------------------------------------------------------------

// EL1 - A simple imparative language
//
// Grammar:
//   Expr -> Int                     
//        |  String                  
//        |  (+ Expr Expr)
//        |  (- Expr Expr)
//        |  (* Expr Expr)
//        |  (/ Expr Expr)
//        |  (% Expr Expr)
//        |  (<= Expr Expr)
//        |  (:= String Expr)        
//        |  (while Expr Expr)     
//        |  (if Expr Expr Expr)  
//        |  (write Expr)         
//        |  (seq Expr Expr)         
//        |  (skip)         	     
//        |  (for String Expr Expr Expr)
//
import SExprLibrary._

object EL1 {
  sealed abstract class Expr {
    override def toString(): String = print(this)
  }
  case class Var(id:String) extends Expr
  case class Num(i:Int) extends Expr
  case class Add(l:Expr,r:Expr) extends Expr
  case class Sub(l:Expr,r:Expr) extends Expr
  case class Mul(l:Expr,r:Expr) extends Expr
  case class Div(l:Expr,r:Expr) extends Expr
  case class Rem(l:Expr,r:Expr) extends Expr
  case class Le(l:Expr,r:Expr) extends Expr
  case class Assgn(id:String,e:Expr) extends Expr
  case class While(c:Expr,e:Expr) extends Expr
  case class If(c:Expr,t:Expr,e:Expr) extends Expr
  case class Write(e:Expr) extends Expr
  case class Seq(e1:Expr,e2:Expr) extends Expr
  case class Skip() extends Expr
  case class For(id:String,e1:Expr,e2:Expr,e3:Expr) extends Expr

  case class ParseException(string: String) extends RuntimeException

  def parse(str:String, debug:Int = 0): Expr = {
    try {
      val a = parseE(SExprReader.read(str))
      if (debug > 0) println("Parsed expression: " + a) 
      a
    } catch {
      case ex:ReadException => throw ParseException(ex.string)
    }	
  }
  
  def parseE(sexpr: SExpr): Expr = sexpr match {
    case SNum(n)  => Num(n)
    case SSym(id) => Var(id)
    case SList(SSym("+") :: l :: r :: Nil)  => Add(parseE(l),parseE(r))
    case SList(SSym("-") :: l :: r :: Nil)  => Sub(parseE(l),parseE(r))
    case SList(SSym("*") :: l :: r :: Nil)  => Mul(parseE(l),parseE(r))
    case SList(SSym("/") :: l :: r :: Nil)  => Div(parseE(l),parseE(r))
    case SList(SSym("%") :: l :: r :: Nil)  => Rem(parseE(l),parseE(r))
    case SList(SSym("<=") :: l :: r :: Nil) => Le(parseE(l),parseE(r))
    case SList(SSym(":=") :: SSym(id) :: e :: Nil) => Assgn(id,parseE(e))
    case SList(SSym("while") :: c :: e :: Nil) => While(parseE(c),parseE(e))
    case SList(SSym("if") :: c :: t :: e :: Nil) 
                                            => If(parseE(c),parseE(t),parseE(e))
    case SList(SSym("write") :: e :: Nil)   => Write(parseE(e))
    case SList(SSym("seq") :: e1 :: e2 :: Nil) => Seq(parseE(e1),parseE(e2))
    case SList(SSym("skip") :: Nil)         => Skip()
    case SList(SSym("for") :: SSym(id) :: e1 :: e2 :: e3 :: Nil) 
                                     => For(id,parseE(e1),parseE(e2),parseE(e3))
    case _ => throw ParseException("Cannot parse expression:" + sexpr)
  }
  
  def print(expr: Expr): String = unparse(expr).toString()

  def unparse(expr: Expr): SExpr = expr match {
    case Num(n)   => SNum(n)
    case Var(x)   => SSym(x)
    case Add(l,r) => SList(SSym("+") :: unparse(l) :: unparse(r) :: Nil)
    case Sub(l,r) => SList(SSym("-") :: unparse(l) :: unparse(r) :: Nil)
    case Mul(l,r) => SList(SSym("*") :: unparse(l) :: unparse(r) :: Nil)
    case Div(l,r) => SList(SSym("/") :: unparse(l) :: unparse(r) :: Nil)
    case Rem(l,r) => SList(SSym("%") :: unparse(l) :: unparse(r) :: Nil)
    case Le(l,r)  => SList(SSym("<=") :: unparse(l) :: unparse(r) :: Nil)
    case Assgn(x,e) => SList(SSym(":=") :: SSym(x) :: unparse(e) :: Nil)
    case While(c,e) => SList(SSym("while") :: unparse(c) :: unparse(e) :: Nil)
    case If(c,t,e)  => SList(SSym("if") :: unparse(c) :: unparse(t) 
                               :: unparse(e) :: Nil)
    case Write(e)   => SList(SSym("write") :: unparse(e) :: Nil)
    case Seq(e1,e2) => SList(SSym("seq") :: unparse(e1) :: unparse(e2) :: Nil)
    case Skip()     => SList(SSym("skip") :: Nil)
    case For(x,e1,e2,e3) => SList(SSym("for") :: SSym(x) :: unparse(e1) 
                                    :: unparse(e2) :: unparse(e3) :: Nil)
  }
  
}    
//

//-------------------------------------------------------------------------
// Supporting code for CS558 Programming Languages. 
// Dept of Computer Science, Portland State University
// Original version: A. Tolmach; modified: J. Li (8/2018)
//-------------------------------------------------------------------------

// EL3 - A functional language
//
// Grammar:
//   Expr -> Int
//        |  String                    
//        |  (+ Expr Expr)
//        |  (- Expr Expr)
//        |  (* Expr Expr)
//        |  (<= Expr Expr)
//        |  (if Expr Expr Expr)  
//        |  (seq Expr Expr)         
//        |  (skip)         	     
//        |  (let String Expr Expr)    
//        |  (letRec String Expr Expr)    
//        |  (fun String Expr)    
//        |  (@ Expr Expr)
//
import SExprLibrary._

object EL3 {
  sealed abstract class Expr {
    override def toString() : String = print(this)
  }
  case class Num(i:Int) extends Expr
  case class Var(id:String) extends Expr
  case class Add(l:Expr,r:Expr) extends Expr
  case class Sub(l:Expr,r:Expr) extends Expr
  case class Mul(l:Expr,r:Expr) extends Expr
  case class Div(l:Expr,r:Expr) extends Expr
  case class Le(l:Expr,r:Expr) extends Expr
  case class If(c:Expr,t:Expr,e:Expr) extends Expr
  case class Seq(e1:Expr,e2:Expr) extends Expr
  case class Skip() extends Expr
  case class Fun(p:String,b:Expr) extends Expr
  case class Apply(f:Expr,e:Expr) extends Expr
  case class Let(id:String,b:Expr,e:Expr) extends Expr
  case class LetRec(id:String,b:Expr,e:Expr) extends Expr

  case class ParseException(string: String) extends RuntimeException

  def parse(str:String,debug:Int = 0): Expr = {
    try {
      val a = parseE(SExprReader.read(str))
      if (debug > 0) println("Parsed expression: " + a) 
      a
    } catch {
      case ex:ReadException => throw ParseException(ex.string)
    }
  }

  def parseE(sexpr: SExpr) : Expr = sexpr match {
    case SNum(n) => Num(n)
    case SSym(id) => Var(id)
    case SList(SSym("+") :: l :: r :: Nil)  => Add(parseE(l),parseE(r))
    case SList(SSym("-") :: l :: r :: Nil)  => Sub(parseE(l),parseE(r))
    case SList(SSym("*") :: l :: r :: Nil)  => Mul(parseE(l),parseE(r))
    case SList(SSym("/") :: l :: r :: Nil)  => Div(parseE(l),parseE(r))
    case SList(SSym("<=") :: l :: r :: Nil) => Le(parseE(l),parseE(r))
    case SList(SSym("if") :: c :: t :: e :: Nil) 
                                     => If(parseE(c),parseE(t),parseE(e))
    case SList(SSym("seq") :: e1 :: e2 :: Nil) => Seq(parseE(e1),parseE(e2))
    case SList(SSym("skip") :: Nil)         => Skip()
    case SList(SSym("let") :: SSym(id) :: e :: b :: Nil) 
                                     => Let(id,parseE(e),parseE(b))
    case SList(SSym("letRec") :: SSym(id) :: b :: e :: Nil) 
                                     => LetRec(id,parseE(b),parseE(e))
    case SList(SSym("fun") :: SSym(p) :: b :: Nil) => Fun(p,parseE(b))
    case SList(SSym("@") :: f :: e :: Nil) => Apply(parseE(f),parseE(e))
    case _ => throw ParseException("Cannot parse expression:" + sexpr)
  }
  
  def print(e: Expr) : String = unparse(e).toString()

  def unparse(expr: Expr) : SExpr = expr match {
    case Num(n)   => SNum(n)
    case Var(x)   => SSym(x)
    case Add(l,r) => SList(SSym("+") :: unparse(l) :: unparse(r) :: Nil)
    case Sub(l,r) => SList(SSym("-") :: unparse(l) :: unparse(r) :: Nil)
    case Mul(l,r) => SList(SSym("*") :: unparse(l) :: unparse(r) :: Nil)
    case Div(l,r) => SList(SSym("/") :: unparse(l) :: unparse(r) :: Nil)
    case Le(l,r)  => SList(SSym("<=") :: unparse(l) :: unparse(r) :: Nil)
    case If(c,t,e)  => SList(SSym("if") :: unparse(c) :: unparse(t) 
                                    :: unparse(e) :: Nil)
    case Seq(e1,e2) => SList(SSym("seq") :: unparse(e1) :: unparse(e2) :: Nil)
    case Skip()     => SList(SSym("skip") :: Nil)
    case Let(x,b,e) => SList(SSym("let") :: SSym(x) :: unparse(b) 
                                    :: unparse(e) :: Nil)
    case LetRec(x,b,e) => SList(SSym("letRec") :: SSym(x) :: unparse(b) 
                                    :: unparse(e) :: Nil)    
    case Fun(x,b) => SList(SSym("fun") :: SSym(x) :: unparse(b) :: Nil)
    case Apply(e1,e2) => SList(SSym("@") :: unparse(e1) :: unparse(e2) :: Nil)
  }
}    

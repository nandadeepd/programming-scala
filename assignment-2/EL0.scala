//-------------------------------------------------------------------------
// Supporting code for CS558 Programming Languages. 
// Dept of Computer Science, Portland State University
// Original version: A. Tolmach; modified: J. Li (8/2018)
//-------------------------------------------------------------------------

// EL0 - A simple expression language
//
// Grammar:
//   Expr -> Int
//        |  (+ Expr Expr)
//        |  (- Expr Expr)
//        |  (* Expr Expr)
//        |  (/ Expr Expr)
//        |  (% Expr Expr)
//
import SExprLibrary._

object EL0 {
  sealed abstract class Expr {
    override def toString(): String = print(this)
  }
  case class Num(n:Int) extends Expr
  case class Add(l:Expr,r:Expr) extends Expr
  case class Sub(l:Expr,r:Expr) extends Expr
  case class Mul(l:Expr,r:Expr) extends Expr
  case class Div(l:Expr,r:Expr) extends Expr
  case class Rem(l:Expr,r:Expr) extends Expr

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
    case SNum(n) => Num(n)
    case SList(SSym("+") :: l :: r :: Nil) => Add(parseE(l),parseE(r))
    case SList(SSym("-") :: l :: r :: Nil) => Sub(parseE(l),parseE(r))
    case SList(SSym("*") :: l :: r :: Nil) => Mul(parseE(l),parseE(r))
    case SList(SSym("/") :: l :: r :: Nil) => Div(parseE(l),parseE(r))
    case SList(SSym("%") :: l :: r :: Nil) => Rem(parseE(l),parseE(r))
    case _ => throw ParseException("Cannot parse:" + sexpr)
  }

  def print(expr: Expr): String = unparse(expr).toString()

  def unparse(expr: Expr): SExpr = expr match {
    case Num(n) => SNum(n)
    case Add(l,r) => SList(SSym("+") :: unparse(l) :: unparse(r) :: Nil)
    case Sub(l,r) => SList(SSym("-") :: unparse(l) :: unparse(r) :: Nil)
    case Mul(l,r) => SList(SSym("*") :: unparse(l) :: unparse(r) :: Nil)
    case Div(l,r) => SList(SSym("/") :: unparse(l) :: unparse(r) :: Nil)
    case Rem(l,r) => SList(SSym("%") :: unparse(l) :: unparse(r) :: Nil)
  }
}    
//

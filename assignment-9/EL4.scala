//-------------------------------------------------------------------------
// Supporting code for CS558 Programming Languages. 
// Dept of Computer Science, Portland State University
// Original version: A. Tolmach; modified: J. Li (8/2018)
//-------------------------------------------------------------------------

// EL4 - A typed functional language
//
// Grammar:
//   Expr -> Int
//        |  Bool                    
//        |  String                    
//        |  (Op Expr Expr)
//        |  (if Expr Expr Expr)  
//        |  (seq Expr Expr)         
//        |  (fun String Type Expr)    
//        |  (@ Expr Expr)
//        |  (let String Type Expr Expr)    
//        |  (letRec String Type Expr Expr)
//   Op   -> + | - | * | / | && | || | <=
//   Type -> intT
//        |  boolT
//        |  (funT Type Type)
//
import SExprLibrary._

object EL4 {
  sealed abstract class Expr {
    override def toString(): String = print(this)
  }
  case class Num(i:Int) extends Expr
  case class Bool(b:Boolean) extends Expr
  case class Var(id:String) extends Expr
  case class Add(l:Expr,r:Expr) extends Expr
  case class Sub(l:Expr,r:Expr) extends Expr
  case class Mul(l:Expr,r:Expr) extends Expr
  case class Div(l:Expr,r:Expr) extends Expr
  case class And(l:Expr,r:Expr) extends Expr
  case class Or(l:Expr,r:Expr) extends Expr
  case class Le(l:Expr,r:Expr) extends Expr
  case class If(c:Expr,t:Expr,e:Expr) extends Expr
  case class Seq(e1:Expr,e2:Expr) extends Expr
  case class Fun(p:String,t:Type,b:Expr) extends Expr
  case class Apply(f:Expr,e:Expr) extends Expr
  case class Let(id:String,t:Type,b:Expr,e:Expr) extends Expr
  case class LetRec(id:String,t:Type,b:Expr,e:Expr) extends Expr

  case class ParseException(string: String) extends RuntimeException

  sealed abstract class Type {
    override def toString(): String = print(this)
  }
  case object IntTy extends Type
  case object BoolTy extends Type
  case class FunTy(pt:Type,rt:Type) extends Type

  def parse(str:String,debug:Int = 0): Expr = {
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
    case SSym("true") => Bool(true)
    case SSym("false") => Bool(false)
    case SSym(id) => Var(id)
    case SList(SSym("+") :: l :: r :: Nil)  => Add(parseE(l),parseE(r))
    case SList(SSym("-") :: l :: r :: Nil)  => Sub(parseE(l),parseE(r))
    case SList(SSym("*") :: l :: r :: Nil)  => Mul(parseE(l),parseE(r))
    case SList(SSym("/") :: l :: r :: Nil)  => Div(parseE(l),parseE(r))
    case SList(SSym("&&") :: l :: r :: Nil) => And(parseE(l),parseE(r))
    case SList(SSym("||") :: l :: r :: Nil) => Or(parseE(l),parseE(r))
    case SList(SSym("<=") :: l :: r :: Nil) => Le(parseE(l),parseE(r))
    case SList(SSym("if") :: c :: t :: e :: Nil) 
                                     => If(parseE(c),parseE(t),parseE(e))
    case SList(SSym("seq") :: e1 :: e2 :: Nil) => Seq(parseE(e1),parseE(e2))
    case SList(SSym("fun") :: SSym(p) :: t :: b :: Nil) => Fun(p,parseT(t),parseE(b))
    case SList(SSym("@") :: f :: e :: Nil) => Apply(parseE(f),parseE(e))
    case SList(SSym("let") :: SSym(id) :: t :: b :: e :: Nil) 
                                     => Let(id,parseT(t),parseE(b),parseE(e))
    case SList(SSym("letRec") :: SSym(id) :: t :: b :: e :: Nil) 
                                     => LetRec(id,parseT(t),parseE(b),parseE(e))
    case _ => throw ParseException("Cannot parse expression:" + sexpr)
  }
  
  def parseT(sexpr: SExpr): Type = sexpr match {
    case SSym("intT") => IntTy
    case SSym("boolT") => BoolTy
    case SList(SSym("funT") :: pt :: rt :: Nil) => FunTy(parseT(pt),parseT(rt))
    case _ => throw ParseException("Cannot parse type expression:" + sexpr)
  }

  def print(e: Expr): String = unparse(e).toString()
  def print(t: Type): String = unparse(t).toString()

  def unparse(expr: Expr): SExpr = expr match {
    case Num(n)      => SNum(n)
    case Bool(true)  => SSym("true")
    case Bool(false) => SSym("false")
    case Var(x)   => SSym(x)
    case Add(l,r) => SList(SSym("+") :: unparse(l) :: unparse(r) :: Nil)
    case Sub(l,r) => SList(SSym("-") :: unparse(l) :: unparse(r) :: Nil)
    case Mul(l,r) => SList(SSym("*") :: unparse(l) :: unparse(r) :: Nil)
    case Div(l,r) => SList(SSym("/") :: unparse(l) :: unparse(r) :: Nil)
    case And(l,r) => SList(SSym("&&") :: unparse(l) :: unparse(r) :: Nil)
    case Or(l,r)  => SList(SSym("||") :: unparse(l) :: unparse(r) :: Nil)
    case Le(l,r)  => SList(SSym("<=") :: unparse(l) :: unparse(r) :: Nil)
    case If(c,t,e) => SList(SSym("if") :: unparse(c) :: unparse(t) 
                                    :: unparse(e) :: Nil)
    case Seq(e1,e2) => SList(SSym("seq") :: unparse(e1) :: unparse(e2) :: Nil)
    case Fun(x,t,b) => SList(SSym("fun") :: SSym(x) :: unparse(t) :: unparse(b) :: Nil)
    case Apply(e1,e2) => SList(SSym("@") :: unparse(e1) :: unparse(e2) :: Nil)
    case Let(x,t,b,e) => SList(SSym("let") :: SSym(x) :: unparse(t) :: unparse(b) 
                                    :: unparse(e) :: Nil)
    case LetRec(x,t,b,e) => SList(SSym("letRec") :: SSym(x) :: unparse(t) :: unparse(b) 
                                    :: unparse(e) :: Nil)    
  }

  def unparse(ty: Type): SExpr = ty match {
    case IntTy  => SSym("intT")
    case BoolTy => SSym("boolT")
    case FunTy(pt,rt) => SList(SSym("funT") :: unparse(pt) :: unparse(rt) :: Nil)
  }
}    
//

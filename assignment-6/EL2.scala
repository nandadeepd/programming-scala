//-------------------------------------------------------------------------
// Supporting code for CS558 Programming Languages. 
// Dept of Computer Science, Portland State University
// Original version: A. Tolmach; modified: J. Li (8/2018)
//-------------------------------------------------------------------------

// EL2 - An extended imparative language (with scopes and pairs)
//
// Grammar:
//   Program -> (List[GDef] Expr)
//   GDef -> (String Expr)
//   Expr -> Int                     
//        |  String                  
//        |  (Op Expr Expr)      // arithmetic ops
//        |  (= Expr Expr)       // shallow comparison
//        |  (== Expr Expr)      // deep comparison
//        |  (:= String Expr)        
//        |  (if Expr Expr Expr)  
//        |  (write Expr)         
//        |  (seq Expr Expr)         
//        |  (skip)         	     
//        |  (let String Expr Expr)
//        |  (pair Expr Expr)         
//        |  (isPair Expr)         
//        |  (fst Expr)         
//        |  (snd Expr)         
//        |  (setFst Expr Expr)         
//        |  (setSnd Expr Expr)         
//   Op   -> + | - | * | <=      
//
import SExprLibrary._

object EL2 {
  case class Program(gdefs:List[GDef], body:Expr) {
    override def toString(): String = print(this)
  }

  case class GDef(id:String, d:Expr) {
    override def toString(): String = print(this)
  }

  sealed abstract class Expr {
    override def toString(): String = print(this)
  }
  case class Var(id:String) extends Expr
  case class Num(i:Int) extends Expr
  case class Add(l:Expr,r:Expr) extends Expr
  case class Sub(l:Expr,r:Expr) extends Expr
  case class Mul(l:Expr,r:Expr) extends Expr
  case class Le(l:Expr,r:Expr) extends Expr
  case class Eq(l:Expr,r:Expr) extends Expr
  case class Deq(l:Expr,r:Expr) extends Expr
  case class Assgn(id:String,e:Expr) extends Expr
  case class If(c:Expr,t:Expr,e:Expr) extends Expr
  case class Write(e:Expr) extends Expr
  case class Seq(e1:Expr,e2:Expr) extends Expr
  case class Skip() extends Expr
  case class Let(id:String,e:Expr,b:Expr) extends Expr
  case class Pair(l:Expr,r:Expr) extends Expr
  case class Fst(e:Expr) extends Expr
  case class Snd(e:Expr) extends Expr
  case class IsPair(e:Expr) extends Expr
  case class SetFst(p:Expr,e:Expr) extends Expr
  case class SetSnd(p:Expr,e:Expr) extends Expr

  case class ParseException(string: String) extends RuntimeException

  def parse(str:String, debug:Int = 0): Program = {
    try {
      val a = parseP(SExprReader.read(str))
      if (debug > 0) println("Parsed program: " + a) 
      a
    } catch {
      case ex:ReadException => throw ParseException(ex.string)
    }	
  }
  
  def parseP(sexpr: SExpr) : Program = sexpr match {
    case SList(SList(gs) :: e :: Nil) => Program(parseGs(gs),parseE(e))
    case _ => throw ParseException("Cannot parse program:" + sexpr)
  }

  def parseG(sexpr: SExpr) : GDef = sexpr match {
    case SList(SSym(id) :: e :: Nil) => GDef(id,parseE(e))
    case _ => throw ParseException("Cannot parse global definition:" + sexpr)
  }

  def parseGs(sexprs : List [SExpr]) : List[GDef] = sexprs match {
    case Nil => Nil
    case (d :: ds) => parseG(d) :: parseGs(ds)
  }

  def parseE(sexpr: SExpr): Expr = sexpr match {
    case SNum(n)  => Num(n)
    case SSym(id) => Var(id)
    case SList(SSym("+") :: l :: r :: Nil)  => Add(parseE(l),parseE(r))
    case SList(SSym("-") :: l :: r :: Nil)  => Sub(parseE(l),parseE(r))
    case SList(SSym("*") :: l :: r :: Nil)  => Mul(parseE(l),parseE(r))
    case SList(SSym("<=") :: l :: r :: Nil) => Le(parseE(l),parseE(r))
    case SList(SSym("=") :: l :: r :: Nil)  => Eq(parseE(l),parseE(r))
    case SList(SSym("==") :: l :: r :: Nil) => Deq(parseE(l),parseE(r))
    case SList(SSym(":=") :: SSym(id) :: e :: Nil) => Assgn(id,parseE(e))
    case SList(SSym("if") :: c :: t :: e :: Nil) 
                                            => If(parseE(c),parseE(t),parseE(e))
    case SList(SSym("write") :: e :: Nil)   => Write(parseE(e))
    case SList(SSym("seq") :: e1 :: e2 :: Nil) => Seq(parseE(e1),parseE(e2))
    case SList(SSym("skip") :: Nil)         => Skip()
    case SList(SSym("let") :: SSym(id) :: e :: b :: Nil) 
                                     => Let(id,parseE(e),parseE(b))
    case SList(SSym("pair") :: l :: r :: Nil) => Pair(parseE(l),parseE(r))
    case SList(SSym("fst") :: e :: Nil)     => Fst(parseE(e))
    case SList(SSym("snd") :: e :: Nil)     => Snd(parseE(e))
    case SList(SSym("isPair") :: e :: Nil)  => IsPair(parseE(e))
    case SList(SSym("setFst") :: p :: e :: Nil) => SetFst(parseE(p),parseE(e))
    case SList(SSym("setSnd") :: p :: e :: Nil) => SetSnd(parseE(p),parseE(e))
    case _ => throw ParseException("Cannot parse expression:" + sexpr)
  }
  
  // These methods are distinguished by the type of their parameter.
  def print(p: Program) : String = unparse(p).toString()
  def print(g: GDef) : String = unparse(g).toString()
  def print(e: Expr) : String = unparse(e).toString()

  def unparse(p: Program) : SExpr =
    SList(SList(unparseGs(p.gdefs)) :: unparse(p.body) :: Nil)

  def unparse(g: GDef) : SExpr = 
    SList(SSym(g.id) :: unparse(g.d) :: Nil) 

  def unparseGs(gs: List[GDef]) : List[SExpr] = gs match {
    case Nil => Nil
    case d :: ds => unparse(d) :: unparseGs(ds)
  }

  def unparse(expr: Expr): SExpr = expr match {
    case Num(n)   => SNum(n)
    case Var(x)   => SSym(x)
    case Add(l,r) => SList(SSym("+") :: unparse(l) :: unparse(r) :: Nil)
    case Sub(l,r) => SList(SSym("-") :: unparse(l) :: unparse(r) :: Nil)
    case Mul(l,r) => SList(SSym("*") :: unparse(l) :: unparse(r) :: Nil)
    case Le(l,r)  => SList(SSym("<=") :: unparse(l) :: unparse(r) :: Nil)
    case Eq(l,r)  => SList(SSym("=") :: unparse(l) :: unparse(r) :: Nil)
    case Deq(l,r) => SList(SSym("==") :: unparse(l) :: unparse(r) :: Nil)
    case Assgn(x,e) => SList(SSym(":=") :: SSym(x) :: unparse(e) :: Nil)
    case If(c,t,e)  => SList(SSym("if") :: unparse(c) :: unparse(t) 
                               :: unparse(e) :: Nil)
    case Write(e)   => SList(SSym("write") :: unparse(e) :: Nil)
    case Seq(e1,e2) => SList(SSym("seq") :: unparse(e1) :: unparse(e2) :: Nil)
    case Skip()     => SList(SSym("skip") :: Nil)
    case Let(x,e,b) => SList(SSym("let") :: SSym(x) :: unparse(e) 
                                    :: unparse(b) :: Nil)
    case Pair(l,r) => SList(SSym("pair") :: unparse(l) :: unparse(r) :: Nil)
    case Fst(e) => SList(SSym("fst") :: unparse(e)  :: Nil)
    case Snd(e) => SList(SSym("snd") :: unparse(e)  :: Nil)
    case IsPair(e) => SList(SSym("isPair") :: unparse(e) :: Nil)
    case SetFst(p,e) => SList(SSym("setFst") :: unparse(p) :: unparse(e) :: Nil) 
    case SetSnd(p,e) => SList(SSym("setSnd") :: unparse(p) :: unparse(e) :: Nil) 
  }
}
//

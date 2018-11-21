// Nandadeep Davuluru, davuluru@pdx.edu
//-------------------------------------------------------------------------

// EL3 Interpreter
//
// Usage: linux> scala Interp3 <source file>
//
import EL3._

object Interp3 {
  case class InterpException(string: String) extends RuntimeException

  var useHeap = false;
  var callByName = false;

  sealed abstract class Addr() {
    def +(offset:Int): Addr
  }
  case class HeapAddr(index:Int) extends Addr {
    def +(offset:Int) = HeapAddr(index+offset)
  }
  case class StackAddr(index:Int) extends Addr {
    def +(offset:Int) = StackAddr(index+offset)
  }

  sealed abstract class Value
  case class NumV(num:Int) extends Value
  case class ClosureV(x:String,b:Expr,env:Env) extends Value

  type Index = Int

  class Store {
    case class UndefinedContents(string: String) extends RuntimeException
    private val contents = collection.mutable.Map[Index,Value]()
    def get(i:Index) = contents.getOrElse(i, throw UndefinedContents("" + i))
    def set(i:Index,v:Value) = contents += (i->v)
    override def toString: String = contents.toString
  }

  class HeapStore extends Store {
    private var nextFreeIndex:Index = 0
    def allocate(n:Int): Addr = {
      val i = nextFreeIndex
      nextFreeIndex += n
      HeapAddr(i)
    }
    // there is no mechanism for deallocation
    override def toString: String = "[next=" + nextFreeIndex + "] " + super.toString
  }

  class StackStore extends Store {
    private var stackPointer:Index = 0;
    def push(): Addr = {
      val i = stackPointer
      stackPointer += 1
      StackAddr(i)
    }
    def pop() = stackPointer -= 1
    override def toString: String = "[sp=" + stackPointer + "] " + super.toString
  }

  type Env = Map[String,Addr]

  val emptyEnv: Env =  Map[String,Addr]() 

  def interp(p:Expr,debug:Int = 0): Int = {
    if (debug > 0) println("expr: " + p)

    val heap = new HeapStore()
    val stack = new StackStore()

    def get(a:Addr) = a match {
      case HeapAddr(i)  => heap.get(i)
      case StackAddr(i) => stack.get(i)
    }

    def set(a:Addr,v:Value) = a match {
      case HeapAddr(i)  => heap.set(i,v)
      case StackAddr(i) => stack.set(i,v)
    }

    def interpVar(env:Env,x:String): Addr = 
      env.getOrElse(x, throw InterpException("undefined variable:" + x))

    def interpArithBinOp(env:Env,l: Expr, r:Expr) (op: (Int,Int) => Int) = {
      val lv = interpE(env,l)
      val rv = interpE(env,r)
      (lv,rv) match {
      case (NumV(ln),NumV(rn)) => NumV(op(ln, rn))
      case _ => throw InterpException("non-numeric argument to arithmetic operator")
      }   
    }

    def interpE(env:Env,e:Expr): Value = {
        def replaceE(e: Expr, x: String, y: Expr): Expr = {
          e match {
            case Num(n) => e
            case Var(x1) => {
              if(x1 == x){
                y
              } else {
                Var(x1)
              }
            }
            case Add(l, r) => Add(replaceE(l, x, y), replaceE(r, x, y))
            case Sub(l, r) => Sub(replaceE(l, x, y), replaceE(r, x, y))
            case Mul(l, r) => Mul(replaceE(l, x, y), replaceE(r, x, y))
            case Div(l, r) => Div(replaceE(l, x, y), replaceE(r, x, y))
            case Le(l, r)  => Le(replaceE(l, x, y), replaceE(r, x, y))
            case Let(id, e1, b) => Let(id, replaceE(e1, x, y), replaceE(b, x, y))
            case Fun(p, b) => Fun(p, replaceE(b, x, y))
            case Apply(f, e1) => Apply(replaceE(f, x, y), replaceE(e1, x, y))
            case LetRec(id, b, e1) => LetRec(id, replaceE(b, x, y), replaceE(e1, x, y))
            case If(c, t, e1) => If(replaceE(c, x, y), replaceE(t, x, y), replaceE(e1, x, y))
            case Skip() => Skip()
            case Seq(e1, e2) => Seq(replaceE(e1, x, y), replaceE(e2, x, y))
          }
      }
      if (debug > 1) {
        println("expr = "+ e)
        println("env = " + env)
        println("stack = " + stack)
        println("heap = " + heap)
      } 
      e match {
        case Num(n) => NumV(n)
        case Var(x) => get(interpVar(env,x))
        case Add(l,r) => interpArithBinOp(env,l,r) ((lv,rv) => lv + rv)
        case Sub(l,r) => interpArithBinOp(env,l,r) ((lv,rv) => lv - rv)  
        case Mul(l,r) => interpArithBinOp(env,l,r) ((lv,rv) => lv * rv)  
        case Div(l,r) => interpArithBinOp(env,l,r) ((lv,rv) => if (rv!=0) (lv/rv) else throw InterpException("divide by zero"))
        case Le(l,r)  => interpArithBinOp(env,l,r) ((lv,rv) => if (lv <= rv) 1 else 0)
        case If(c,t,e) => interpE(env,c) match {
          case NumV(i) => {
            if (i != 0) {
              interpE(env, t)
            } else {
              interpE(env, e)
            }
          }
          case _ => throw InterpException("non Int to If")

        }
        case Seq(e1,e2) => {
          val v1 = interpE(env,e1)
          val v2 = interpE(env,e2)
          v2
        }
        case Skip() => NumV(0)

        case Let(x, e, b) => {
          var x_local = interpE(env, e);
          if(useHeap) {
            var hAddr = heap.allocate(1);
            set(hAddr, x_local)
            val new_env = env + (x -> hAddr)
            val retval = interpE(new_env, b)
            retval
          }
          else 
          {
            // stack 
            var sAddr = stack.push();
            set(sAddr, x_local)
            val new_env = env + (x -> sAddr)
            val retval = interpE(new_env, b)
            stack.pop()
            retval
          } 
        }
        case LetRec(f, b, e) => {
          if(useHeap) {
            val f_hAddr = heap.allocate(1);
            val env_with_f = env + (f -> f_hAddr)
            val b_closure = interpE(env_with_f, b)
            b_closure match {
            case ClosureV(x, innerB, cenv) => {
              set(f_hAddr, b_closure)
              } 
            case _ => throw InterpException("B is not a closure")
            }
            val retval = interpE(env_with_f, e)
            retval
          } else {
            // stack 
            val f_addr = stack.push();
            val env_with_f = env + (f -> f_addr)
            val b_closure = interpE(env_with_f, b)
            b_closure match {
              case ClosureV(x, innerB, cenv) => {
                set(f_addr, b_closure)
              } 
              case _ => throw InterpException("B is not a closure")
            }
            val retval = interpE(env_with_f, e)
            stack.pop()
            retval
          }
        }

        case Fun(x,b) => {
          ClosureV(x, b, env)
        }

        case Apply(f, e) => interpE(env, f) match {
          case ClosureV(pname, expr, cenv) => {
            if(callByName) {
              // performing beta reduction
              val replaced_expr = replaceE(expr, pname, e)
              val some = interpE(cenv, replaced_expr); some
            } else {
              if(useHeap) {
                val hSomeAddr = heap.allocate(1)
                val outer_expr = interpE(env, e)
                set(hSomeAddr, outer_expr)
                val local_env = cenv + (pname -> hSomeAddr)
                val some = interpE(local_env, expr) 
                some
              } else {
                val someAddr = stack.push()
                val outer_expr = interpE(env, e)
                set(someAddr, outer_expr)
                // using local scope for evaluating expr
                val local_env = cenv + (pname -> someAddr)
                val some = interpE(local_env, expr)
                stack.pop(); 
                some
            }
            
            
            
            
            
            }
          }
          case _ => throw new InterpException("not a closure")

        }

      }
    }

    // process the top-level expression
    val v = interpE(emptyEnv,p)
    if (debug > 0) println("Expression evaluates to: " + v)
    v match {
      case NumV(n) => n
      case _ => throw InterpException("main body returns non-integer")
    }
  }

  def process(s:String,heap:Boolean=false,cbn:Boolean=false,debug:Int=0): Int = {
    try {
      val p: Expr = parse(s,debug)
      useHeap = heap;
      callByName = cbn;
      interp(p,debug)
    } catch {
      case ex: InterpException => { println("Interp Error:" + ex.string) ; throw ex }
      case ex: ParseException => { println("Parser Error:" + ex.string) ; throw ex }
    }
  }

  // Test driver
  import scala.io.Source
  def main(argv: Array[String]) = {
    val s = Source.fromFile(argv(0)).getLines.mkString("\n")
    var heap = false
    var cbn = false
    var debug = 0
    for (arg <- argv) {
      if (arg == "heap") heap = true
      if (arg == "cbn") cbn = true
      if (arg == "1") debug = 1
      if (arg == "2") debug = 2
    }
    val v = process(s,heap,cbn,debug)
    println(v)
  }
}

//

//-------------------------------------------------------------------------
// Supporting code for CS558 Programming Languages. 
// Dept of Computer Science, Portland State University
// Original version: A. Tolmach; modified: J. Li (8/2018)
//-------------------------------------------------------------------------

// Stack Machine (version 1)
//

object Machine1 {

  sealed abstract class Instr
  case class Const(n:Int) extends Instr
  case object Plus extends Instr
  case object Times extends Instr
  case object Divrem extends Instr
  case object Lessequ extends Instr
  case object Pop extends Instr
  case object Dup extends Instr
  case object Swap extends Instr
  case class Load(x:String) extends Instr
  case class Store(x:String) extends Instr
  case object Print extends Instr
  case class Label(l:Int) extends Instr
  case class Branch(l:Int) extends Instr
  case class Branchz(l:Int) extends Instr

  case class ExecException(string: String) extends RuntimeException

  type Program = List[Instr]

  type Stack = collection.mutable.Stack[Int]

  // see http://docs.scala-lang.org/overviews/collections/maps.html
  // for details of Map class
  type VarStore = collection.mutable.Map[String,Int]

  def exec(prog:Program,debug: Int = 0): Int = {
    val stk: Stack = collection.mutable.Stack[Int]()
    val store: VarStore = collection.mutable.Map[String,Int]()  
    var pc = 0

    def step(): Int = 
      prog(pc) match {
        case Const(i) => {
          stk.push(i)
          pc+1
        }
        case Plus => {
          val v2 = stk.pop()
          val v1 = stk.pop()
          stk.push(v1 + v2)
          pc+1
        }
        case Times => {
          val v2 = stk.pop()
          val v1 = stk.pop()
          stk.push(v1 * v2)
          pc+1
        }
        case Divrem => {
          val v2 = stk.pop()
          val v1 = stk.pop()
          if (v2 == 0) 
            throw ExecException("division by zero")
          else {
            stk.push(v1 / v2)
            stk.push(v1 % v2)
          }
          pc+1
        }
        case Lessequ => {
          val v2 = stk.pop()
          val v1 = stk.pop()
          stk.push(if (v1 <= v2) 1 else 0)
          pc+1
        }
        case Pop => {
          stk.pop()
          pc+1
        }
        case Dup => {
          val v = stk.pop()
          stk.push(v)
          stk.push(v)
          pc+1
        }
        case Swap => {
          val v2 = stk.pop()
          val v1 = stk.pop()
          stk.push(v2)
          stk.push(v1)
          pc+1
        }
        case Load(x) => {
          store get x match {
            case Some(v) => stk.push(v)
            case None => stk.push(0)
          }
          pc+1
        }
        case Store(x) => {
          val v = stk.pop()
          store(x) = v
          pc+1
        }
        case Print => {
          val v = stk.pop()
          println(v)
          pc+1
        }
        case Label(l) =>
          pc+1
        case Branch(l) =>
          findLabel(prog,l)
        case Branchz(l) => {
          val v = stk.pop()
          if (v == 0)
            findLabel(prog,l)
          else
            pc+1
        }
      }
    
    def findLabel(prog:Program,l:Int) = {
      def f(n:Int,prog:Program): Int = prog match {
        case Nil => throw ExecException("missing label " + l)
        case Label(l1)::rest if l == l1 => n
        case _::rest => f(n+1,rest)
      }
      f(0,prog)
    }

    if (debug > 0) println("Machine code:" + prog)

    while (pc < prog.length) {
      if (debug > 1) print("" + pc + "*" + prog(pc))
      pc = step()
      if (debug > 1) println (":" + stk.mkString(" "))
    }
    val r = stk.pop()

    // sanity check
    if (!stk.isEmpty)
      throw ExecException("stack not empty at the end of program exection")
    
    if (debug > 0) println("Result:" + r)
    r
  }        
}


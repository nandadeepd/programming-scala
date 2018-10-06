//-------------------------------------------------------------------------
// Supporting code for CS558 Programming Languages. 
// Dept of Computer Science, Portland State University
// Original version: A. Tolmach; modified: J. Li (8/2018)
//-------------------------------------------------------------------------

// Stack Machine (version 0)
//

object Machine0 {

  sealed abstract class Instr
  case class Const(n:Int) extends Instr
  case object Plus extends Instr
  case object Subtract extends Instr
  case object Times extends Instr
  case object Divrem extends Instr
  case object Pop extends Instr
  case object Swap extends Instr

  case class ExecException(string: String) extends RuntimeException

  type Program = List[Instr]

  def step(stk: scala.collection.mutable.Stack[Int], instr: Instr) = 
      instr match {
      case Const(i) => stk.push(i)
      case Plus => {
        val v2 = stk.pop()
        val v1 = stk.pop()
      	stk.push(v1 + v2)
      }
      // subtract function to the stack API
      case Subtract => {
        val v2 = stk.pop()
        val v1 = stk.pop()
        stk.push(v1 - v2)
      }   
      case Times => {
        val v2 = stk.pop()
        val v1 = stk.pop()
        stk.push(v1 * v2)
      }
      case Divrem => {
        val v2 = stk.pop()
        val v1 = stk.pop()
        if (v2 == 0) {
          throw ExecException("division by zero")
        } else {
          stk.push(v1/v2)
          stk.push(v1 % v2)
        }
      }
      case Pop => stk.pop()
      case Swap => {
        val v2 = stk.pop()
        val v1 = stk.pop()
        stk.push(v2)
        stk.push(v1)
      }
  }     

  def exec(p:Program, debug:Int = 0): Int = {
    val stk = new scala.collection.mutable.Stack[Int]()    
    def steps(instrs: List[Instr]): Int = instrs match {
      case Nil => stk.pop()
      case instr::instrs => {
        step(stk,instr)
        if (debug > 1) println("*" + instr + ":" + (stk.mkString(" ")))
          steps(instrs)
      }
    }
    steps(p)
  }        
}


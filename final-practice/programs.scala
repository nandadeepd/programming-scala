// practice programs for finals 
// This file is purely stress programming!
class ScalaProgrammer {
    def who() = "Scala Programmer"
    def code() = "code"
    def hacks() = "hacks" + code()

}

class OOProgrammer extends ScalaProgrammer {
    override def who() = "OOP"
    override def code() = "classes"
}

class FunctionalProgrammer extends ScalaProgrammer {
    override def who() = "FP"
    override def code() = "uses pattern matching"
}

object Finals {
    // polymorphic function for finding the length of abstract lists. 
    def length[T](l: List[T]): Int = l match {
        case (h :: t) => length(t) + 1
        case _ => 0
    }

    def main(args: Array[String]) {
        // testing length 
        val intList = 1 :: 2 :: 3 :: Nil
        val strList = "a" :: "b" :: Nil
        println(length(intList))
        println(length(strList))
        
        // testing dynamic dispatch
        val team = List(new OOProgrammer(), new FunctionalProgrammer())
        for (p <- team) println(p.who() + " " + p.hacks())

    }
}

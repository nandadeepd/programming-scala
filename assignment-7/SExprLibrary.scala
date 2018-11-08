//-------------------------------------------------------------------------
// Supporting code for CS558 Programming Languages. 
// Dept of Computer Science, Portland State University
// Original version: A. Tolmach
//-------------------------------------------------------------------------

// A library for converting between SExpr's internal and external forms
//
object SExprLibrary {
  import scala.util.parsing.combinator._
  import collection.immutable.StringOps

  sealed abstract class SExpr {
    override def toString(): String = SExprPrinter.print(this)
  }
  case class SNum(num: Int) extends SExpr
  case class SSym(symbol: String) extends SExpr
  case class SList(list: List[SExpr]) extends SExpr
  case class SString(str: String) extends SExpr

  case class ReadException(string: String) extends RuntimeException

  object SExprReader extends JavaTokenParsers {

    def read(text: String): SExpr = {
      val result = parseAll(sexpr, uncomment(text)) 
      result match {
        case Success(r, _) => r
        case Failure(msg, n) =>
          throw ReadException("Bad SExpr:" + msg + 
            " (input left: \"" + n.source.toString.drop(n.offset) + "\")")
        case Error(msg, n) =>
          throw ReadException("Bad SExprr:" + msg + 
            " (input left: \"" + n.source.toString.drop(n.offset) + "\")")
      }
    }

    def sexpr: Parser[SExpr] = (num | symbol | slist | string)

    def num: Parser[SExpr] = wholeNumber ^^ { s => SNum(s.toInt) }
    def symbol: Parser[SExpr] = not(wholeNumber | stringLiteral) ~> 
                                  "[^\"()\\s]+".r ^^ SSym
    def slist: Parser[SExpr] = "(" ~> sexpr.* <~ ")" ^^ SList
    def string: Parser[SExpr] = stringLiteral ^^ {s => SString(unquote(s)) }

    def uncomment(str: String): String = {
      def f(ds:(Int,String), c:Char) = {
        val (d,s) = ds
	c match {
        case '{' => (d+1,s)
	case '}' if d > 0 => (d-1,s)
	case _ if d == 0 => (d,s :+ c)
	case _ => (d,s)
	}
      }
      val (_,s) = ((0,"") /: str) (f)
      s
    }

    def unquote(str: String): String = {
      if (str != null && str.length >= 2 && str.charAt(0) == '\"' && 
          str.charAt(str.length - 1) == '\"')
        str.substring(1, str.length - 1)
      else
        sys.error("unquote inconsistency:" + str)
    }
  }

  object SExprPrinter {
    def print(sexpr: SExpr): String = sexpr match {
      case SSym(str) => str
      case SNum(n)   => n.toString
      case SList(l)  => "(" ++ l.map(print).mkString(" ") ++ ")"
      case SString(str) => "\"" + str + "\""
    }
  }

}

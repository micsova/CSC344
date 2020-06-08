import scala.util.matching.Regex
import scala.io.StdIn.readLine

// Unambiguous grammar
// E -> T '|' E | T
// T -> F T | F
// F -> A '?' | A
// A -> C | '(' E ')'
// C -> '0' | '1' | ... | '9' | 'a' | 'b' | ... | 'z' | '.'

// Easier to parse grammar
// S -> E$
// E -> T E2
// E2 -> '|' E
// E2 -> NIL
// T -> F T2
// T2 -> T
// T2 -> NIL
// F -> A F2
// F2 -> '?' F2
// F2 -> NIL
// A -> C
// A -> '(' A2
// A2 -> E ')'

abstract class S
case class E(left: T, right: Option[E2]) extends S
case class E2(left: E) extends S
case class T(left: F, right: Option[T2]) extends S
case class T2(left: T) extends S
case class F(left: A, right: Option[F2]) extends S
case class F2(left: Option[F2]) extends S
case class A(left: Either[A2, C]) extends S
case class A2(left: E) extends S
case class C(left: Char) extends S

class RecursiveDescent(input:String) {
  val characterRegex: Regex = "^[a-zA-Z0-9]+".r
  val period: Char = '.'
  val space: Char = ' '

  var index = 0

  def parseS(): S = parseE()

  def parseE(): E = E(parseT(), parseE2())

  def parseE2(): Option[E2] = {
    if (index < input.length && input(index) == '|'){
      index+=1; // Advance past |
      Some(E2(parseE()))
    }
    else None
  }

  def parseT(): T = T(parseF(), parseT2())

  def parseT2(): Option[T2] = {
    if (index < input.length && input.charAt(index) != '|' && input.charAt(index) != ')') {
      Some(T2(parseT()))
    }
    else None
  }

  def parseF(): F = F(parseA(), parseF2())

  def parseF2(): Option[F2] = {
    if (index < input.length && input(index) == '?'){
      index+=1; // Advance past ?
      Some(F2(parseF2()))
    }
    else None
  }

  def parseA(): A = {
    if(index < input.length && input(index) == '('){
      index+=1; // Advance past (
      val chars = A2(parseE())
      index+=1; // Advance past )
      A(Left(chars))
    }
    else A(Right(parseC()))
  }

  def parseC(): C = {
    val currStr: String = input.substring(index)
    val consts = characterRegex.findAllIn(currStr)
    if(consts.hasNext) {
      val character = input.charAt(index)
      index+=1
      C(character)
    } else if(currStr.charAt(0) == period) {
      index+=1
      C(period)
    } else if(currStr.charAt(0) == space) {
      index+=1
      C(space)
    } else null
  }
}

object Main {
  type Environment = String => Int
  var index = 0;

  def main(args: Array[String]) {
    val pattern = readLine("pattern? ")
    val rd = new RecursiveDescent(pattern)
    val parsedPattern:S = rd.parseS()
    var string = readLine("string? ")
    while(string != "quit") {
      index = 0;
      val result = matchChecker(parsedPattern, string)
      val atEnd = index == string.length();
      if(result && atEnd) println("match")
      else println("no match")
      string = readLine("string? ")
    }
  }

  def matchChecker(pattern: S, input: String): Boolean = {
    pattern match {
      case e: E =>
        e.right match {
          case Some(right) =>
            val startingIndex = index;
            val l = matchChecker(e.left, input)
            if(l) true
            else {
              index = startingIndex
              matchChecker(right, input)
            }
          case None => matchChecker(e.left, input)
        }
      case e2: E2 => matchChecker(e2.left, input)
      case t: T =>
        t.right match {
          case Some(right) => matchChecker(t.left, input) && matchChecker(right, input)
          case None => matchChecker(t.left, input)
        }
      case t2: T2 => matchChecker(t2.left, input)
      case f: F =>
        f.right match {
          case Some(_) =>
            val startingIndex = index;
            val l = matchChecker(f.left, input)
            if(l) true
            else {
              index = startingIndex
              true
            }
          case None => matchChecker(f.left, input)
        }
      case f2: F2 =>
        f2.left match {
          case Some(left) => matchChecker(left, input)
          case None => true
        }
      case a: A =>
        a.left match {
          case Left(l) => matchChecker(l, input)
          case Right(l) => matchChecker(l, input)
        }
      case a2: A2 => matchChecker(a2.left, input)
      case c: C =>
        if(index >= input.length) false
        else {
          if (input.charAt(index).equals(c.left)) {
            index += 1
            true
          } else if (c.left.equals('.')) {
            index += 1
            true
          } else false
        }
    }
  }
}
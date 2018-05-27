import scala.collection.mutable.ListBuffer
import scala.util.Try
import cats.free.Free
import cats.free.Free.liftF

sealed trait Element // TODO: define better name

sealed trait Atom extends Element
object Atom {
  case class IntegerAtom(value: Int) extends Atom
  case class DoubleAtom(value: Double) extends Atom
  case class StringAtom(value: String) extends Atom
  def apply(value: String): Atom = { // TODO: should I handle integers as well?
    Try(DoubleAtom(value.toDouble))
      .getOrElse(StringAtom(value))
  }
}
case class Expression(values: Seq[Element]) extends Element

sealed trait Instruction
case class Multiply(x: Double, y: Double) extends Instruction

object Main extends App {
  import Atom._

  val program = "(* 1 2)"

  def tokenize(program: String): List[String] = {
    program
      .replace("(", " ( ")
      .replace(")", " ) ")
      .split(" +")
      .filter(_.nonEmpty)
      .toList
  }

  def parse(program: String): Element = readFromTokens(tokenize(program))

  def readFromTokens(tokens: List[String]): Element = {
    var remainingTokens = tokens

    def readThatShit(): Element = {
      if (remainingTokens.isEmpty) throw new IllegalArgumentException("unexpected EOF")
      val token = remainingTokens.head
      remainingTokens = remainingTokens.tail
      token match {
        case "(" => {
          val L = new ListBuffer[Element]()
          while (remainingTokens(0) != ")") {
            L += readThatShit()
          }
          remainingTokens = remainingTokens.tail
          Expression(L)
        }
        case ")"  => throw new IllegalArgumentException("unexpected )")
        case atom => Atom(atom)
      }
    }
    readThatShit()
  }

  def parseElement(element: Element): Option[Instruction] = element match {
    case e: Expression => parseExpression(e)
    case _             => ???
  }

  def parseExpression(expression: Expression): Option[Instruction] = expression.values.toList match {
    case StringAtom("*") :: DoubleAtom(x) :: DoubleAtom(y) :: Nil => Some(Multiply(x, y))
    case _ => None
  }

  def eval(parsedProgram: Seq[Element]): Unit = ???

  val parsed = parse(program)
  println(parsed)
  println(parseElement(parsed))
}

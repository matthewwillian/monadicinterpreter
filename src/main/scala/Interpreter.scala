package monadicinterpreter

import cats.data.ReaderWriterState
import cats.instances.string._
import cats.Monoid
import cats.Monad

object Interpreter {
  type Location = Int
  type Index = List[String]

  case class Stack(values: List[Int]) {
    def fetch(loc: Location): Int = values(loc)

    def put(loc: Location, value: Int): Stack = Stack(values.updated(loc, value))

    def push(value: Int): Stack = Stack(value :: values)

    def pop: Stack = Stack(values.tail)
  }

  type StOut[A] = ReaderWriterState[Unit, String, Stack, A]

  object StOut {
    def pure[A](a: A): StOut[A] = ReaderWriterState.pure[Unit, String, Stack, A](a)
  }

  object MonadicActions {
    def getFrom(i: Location): StOut[Int] =
      StOut
        .pure(())
        .get
        .map(_.fetch(i))

    def write(i: Location, value: Int): StOut[Unit] =
      StOut.pure(())
        .modify(_.put(i, value))

    def push(value: Int): StOut[Unit] =
      StOut.pure(())
        .modify(_.push(value))

    def pop: StOut[Unit] =
      StOut.pure(())
        .modify(_.pop)

    def output(a: Any) =
      StOut.pure(())
        .tell(a.toString())
  }

  def eval(exp: Expression, index: Index): StOut[Int] = {
    exp match {
      case Constant(n) => StOut.pure(n)
      case Variable(x) => MonadicActions.getFrom(index.indexOf(x))
      case Minus(x, y) =>
        for {
          a <- eval(x, index)
          b <- eval(y, index)
        } yield (a - b)
      case Greater(x, y) =>
        for {
          a <- eval(x, index)
          b <- eval(y, index)
        } yield {
          if (a > b) 1
          else 0
        }
      case Times(x, y) =>
        for {
          a <- eval(x, index)
          b <- eval(y, index)
        } yield (a * b)
    }
  }

  def interpret(command: Command, index: Index): StOut[Unit] = {
    command match {
      case Assign(name, value) =>
        val loc = index.indexOf(name)
        for {
          v <- eval(value, index)
          _ <- MonadicActions.write(loc, v)
        } yield ()
      case Sequence(s1, s2) =>
        for {
          _ <- interpret(s1, index)
          _ <- interpret(s2, index)
        } yield ()
      case Cond(e, s1, s2) =>
        for {
          x <- eval(e, index)
          _ <- interpret(if (x == 1) s1 else s2, index)
        } yield ()
      case While(e, b) => {
        def loop2(): StOut[Unit] = { // TODO: figure out how to make this work with for notation
          eval(e, index).flatMap {
            case 1 => interpret(b, index).flatMap((_) => loop2())
            case 0 => StOut.pure(())
          }
        }
        loop2()
      }
      case Declare(name, expr, statement) =>
        for {
          v <- eval(expr, index)
          _ <- MonadicActions.push(v)
          _ <- interpret(statement, name :: index)
          _ <- MonadicActions.pop
        } yield ()
      case Print(e) =>
        for {
          v <- eval(e, index)
          _ <- MonadicActions.output(v)
        } yield ()
    }
  }

  def apply(command: Command): String = interpret(command, List()).runL(Unit, Stack(List())).value
}

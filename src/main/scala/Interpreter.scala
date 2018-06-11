package monadicinterpreter

import cats.data.ReaderWriterState
import cats.instances.string._
import cats.Monoid
import cats.Monad

object Interpreter {
  private type Location = Int
  private type Index = List[String]
  private type Stack = List[Int]

  // TODO: rewrite these using std library methods
  private def position(name: String, index: Index) = index.indexOf(name)

  private def fetch(loc: Location, stack: Stack): Int = stack(loc)

  private def put(loc: Location, value: Int, stack: Stack): Stack = stack.updated(loc, value)

  type StOut[A] = ReaderWriterState[Unit, String, Stack, A]

  object StOut {
    def apply[A](f: Stack => (String, Stack, A)): StOut[A] = {
      ReaderWriterState { case ((), stack: Stack) => f(stack) }
    }

    def pure[A](a: A): StOut[A] = ReaderWriterState.pure[Unit, String, Stack, A](a)
  }

  object Actions {
    def getFrom(i: Location): StOut[Int] = StOut(ns => ("", ns, fetch(i, ns)))

    def write(i: Location, value: Int): StOut[Unit] = StOut(ns => ("", put(i, value, ns), ()))

    def push(value: Int): StOut[Unit] = StOut(ns => ("", value :: ns, ()))

    def pop: StOut[Unit] = StOut {
      case _ :: ns => ("", ns, ())
      case Nil     => throw new IllegalStateException("tried to pop from empty stack")
    }

    def output(a: Any) = StOut(n => (a.toString, n, ()))
  }

  def eval(exp: Expression, index: Index): StOut[Int] = {
    exp match {
      case Constant(n) => StOut.pure(n)
      case Variable(x) => Actions.getFrom(position(x, index))
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
        val loc = position(name, index)
        for {
          v <- eval(value, index)
          _ <- Actions.write(loc, v)
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
          _ <- Actions.push(v)
          _ <- interpret(statement, name :: index)
          _ <- Actions.pop
        } yield ()
      case Print(e) =>
        for {
          v <- eval(e, index)
          _ <- Actions.output(v)
        } yield ()
    }
  }
}

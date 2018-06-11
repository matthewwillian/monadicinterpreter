package monadicinterpreter

import scala.util.parsing.combinator._

trait Combinators extends RegexParsers {
  implicit class Chain[T](parser: Parser[T]) {
    def chain(op: Parser[T => T => T]): Parser[T] = {
      for {
        x <- parser
        fys <- many {
          for {
            f <- op
            y <- parser
          } yield (f, y)
        }
      } yield {
        fys.foldLeft(x) { case (x, (f, y)) => f(x)(y) }
      }
    }
  }

  def many[A](parser: Parser[A]): Parser[List[A]] = {
    val parseMore = for {
      x <- parser
      xs <- many(parser)
    } yield (x :: xs)
    parseMore | "" ^^ { _ => Nil }
  }
}

trait ExpressionParsers extends Combinators {
  def number: Parser[Int] = """(0|[1-9]\d*)""".r ^^ { _.toInt }
  def identifier: Parser[String] = """[a-zA-Z]+""".r ^^ { _.toString }

  def rexp: Parser[Expression] = expr.chain(">" ^^ { _ => x: Expression => y: Expression => Greater(x, y) })
  def expr: Parser[Expression] = term.chain("-" ^^ { _ => x: Expression => y: Expression => Minus(x, y) })
  def term: Parser[Expression] = factor.chain("*" ^^ { _ => x: Expression => y: Expression => Times(x, y) })
  def factor: Parser[Expression] =
    identifier ^^ { Variable(_) } |
      number ^^ { Constant(_) } |
      "(" ~ rexp ~ ")" ^^ { case _ ~ r ~ _ => r }
}

trait CommandParsers extends ExpressionParsers {
  def command: Parser[Command] = assign | seqv | whileCommand | declare | printe
  def assign: Parser[Command] = identifier ~ ":=" ~ rexp ^^ { case i ~ _ ~ r => Assign(i, r) }
  def seqv: Parser[Command] = "{" ~ command ~ ";" ~ command ~ "}" ^^ {
    case _ ~ c1 ~ _ ~ c2 ~ _ => Sequence(c1, c2)
  }
  def whileCommand: Parser[Command] = "while" ~ rexp ~ "do" ~ command ^^ {
    case _ ~ e ~ _ ~ c => While(e, c)
  }

  def declare: Parser[Command] = "declare" ~ identifier ~ "=" ~ rexp ~ "in" ~ command ^^ {
    case _ ~ i ~ _ ~ e ~ _ ~ c => Declare(i, e, c)
  }
  def printe: Parser[Command] = "print" ~ rexp ^^ { case _ ~ e => Print(e) }
}

object LanguageParsers extends CommandParsers


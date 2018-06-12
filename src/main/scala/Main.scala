package monadicinterpreter

import scala.util.parsing.combinator._

object Main extends App {
  def parse(program: String): Command =
    LanguageParsers
      .parse(LanguageParsers.command, program)
      .get

  def interpret(command: Command): String = Interpreter.apply(command)

  def run(program: String) = {
    val pipeline =
      parse _ andThen
        interpret andThen
        println

    pipeline(program)
  }
}

package monadicinterpreter

import scala.util.parsing.combinator._

object TestData {
  val program = """
declare x = 150 in
    declare y = 200 in
     {while x > 0 do { x:=x-1; y:=y-1 };
       print y
     }
"""
  val ast = Declare("x", Constant(150),
    Declare("y", Constant(200),
      Sequence(
        While(
          Greater(Variable("x"), Constant(0)),
          Sequence(
            Assign("x", Minus(Variable("x"), Constant(1))),
            Assign("y", Minus(Variable("y"), Constant(1)))
          )
        ),
        Print(Variable("y"))
      )))
}

object Main extends App {
  def parse(program: String): Command =
    LanguageParsers
      .parse(LanguageParsers.command, program)
      .get

  def interpret(command: Command) =
    Interpreter.apply(_)

  def run(program: String) = {
    val pipeline =
      parse _ andThen
        interpret andThen
        println

    pipeline(program)
  }
}

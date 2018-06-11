package monadicinterpreter

object Main extends App {
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
  val parsedProgram = LanguageParser.parse(LanguageParser.command, program)
  println(Environment.interpret(parsedProgram.get, List()).runL(Unit, List()).value)
}

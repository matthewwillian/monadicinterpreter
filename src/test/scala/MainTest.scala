package monadicinterpreter

import org.scalatest._

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

class MainTest extends WordSpec with Matchers {
  "test string" should {
    "parse to the correct ast" in {
      Main.parse(TestData.program) should equal(TestData.ast)
    }
  }

  "ast" should {
    "evaluate to the correct value" in {
      Main.interpret(TestData.ast) should equal("50")
    }
  }
}

package monadicinterpreter

sealed trait Expression
case class Constant(value: Int) extends Expression
case class Variable(value: String) extends Expression
case class Minus(x: Expression, y: Expression) extends Expression
case class Greater(x: Expression, y: Expression) extends Expression
case class Times(x: Expression, y: Expression) extends Expression

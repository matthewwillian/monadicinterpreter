package monadicinterpreter

sealed trait Command
case class Assign(name: String, value: Expression) extends Command
case class Sequence(a: Command, b: Command) extends Command
case class Cond(exp: Expression, a: Command, b: Command) extends Command
case class While(exp: Expression, com: Command) extends Command
case class Declare(name: String, value: Expression, com: Command) extends Command
case class Print(exp: Expression) extends Command

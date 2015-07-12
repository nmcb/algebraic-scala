package playground.interpreter

trait Expr
case class Val(value: Int) extends Expr
case class Add(lhs: Expr, rhs: Expr) extends Expr
case class Sub(lhs: Expr, rhs: Expr) extends Expr

object Expr {
  def interpret(expr: Expr): Int = expr match {
    case Val(v)        => v
    case Add(lhs, rhs) => interpret(lhs) + interpret(rhs)
    case Sub(lhs, rhs) => interpret(lhs) - interpret(rhs)
  }
}
object algebraic {
  trait Expr
  case class Val(value: Int) extends Expr
  case class Add(lhs: Expr, rhs: Expr) extends Expr
  case class Sub(lhs: Expr, rhs: Expr) extends Expr

  object Expr {
    val expr1 = Add(Val(3), Val(4))
    val expr2 = Sub(expr1, Val(2))
    val expr3 = Add(Val(3), Sub(Val(2), Val(4)))
    def interpret(expr: Expr): Int = expr match {
      case Val(v)        => v
      case Add(lhs, rhs) => interpret(lhs) + interpret(rhs)
      case Sub(lhs, rhs) => interpret(lhs) - interpret(rhs)
    }
  }

  trait Instr
  case class PushInstr(value: Int) extends Instr
  case object AddInstr extends Instr
  case object SubInstr extends Instr

  object Instr {
    def compile(e: Expr): List[Instr] = e match {
      case Val(v)      => List(PushInstr(v))
      case Add(e1, e2) => compile(e1) ++ compile(e2) ++ List(AddInstr)
      case Sub(e1, e2) => compile(e1) ++ compile(e2) ++ List(SubInstr)
    }
  }

  case class ExprAlgebra[E](value: Int => E, add: E => E => E, sub: E => E => E)

  object ExprAlgebra {
    val interpreter = ExprAlgebra[Int](
      i => i,
      x => y => x + y,
      x => y => x - y
    )
    val compiler    = ExprAlgebra[List[Instr]](
      i => List(PushInstr(i)),
      x => y => x ++ y ++ List(AddInstr),
      x => y => x ++ y ++ List(SubInstr)
    )
    def foldExpr[E](alg: ExprAlgebra[E])(e: => Expr): E = {
      val ExprAlgebra(value, add, sub) = alg
      e match {
        case Val(i)    => value(i)
        case Add(x, y) => add(foldExpr(alg)(x))(foldExpr(alg)(y))
        case Sub(x, y) => sub(foldExpr(alg)(x))(foldExpr(alg)(y))
      }
    }
    def compile = foldExpr(compiler) _
    def interpret = foldExpr(interpreter) _
  }

  def run() {
    
    import Expr._
    import Instr._
    println("Expressions:")
    println("exp1:" + expr1)
    println("exp2:" + expr2)
    println("exp3:" + expr3)
    println()
    println("Direct interpretation:")
    println("exp1 = " + interpret(expr1))
    println("exp2 = " + interpret(expr2))
    println("exp3 = " + interpret(expr3))
    println()
    println("Direct compilation into instructions:")
    println("exp1 = " + compile(expr1))
    println("exp2 = " + compile(expr2))
    println("exp3 = " + compile(expr3))
    println()
    println("Algebraic interpretation:")
    println("exp1 = " + ExprAlgebra.interpret(expr1))
    println("exp2 = " + ExprAlgebra.interpret(expr2))
    println("exp3 = " + ExprAlgebra.interpret(expr3))
    println()
    println("Algebraic compilation into instructions:")
    println("exp1 = " + ExprAlgebra.compile(expr1))
    println("exp2 = " + ExprAlgebra.compile(expr2))
    println("exp3 = " + ExprAlgebra.compile(expr3))
  }  
}




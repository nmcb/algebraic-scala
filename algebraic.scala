object algebraic {
  trait Expr
  case class Val(value: Int) extends Expr
  case class Add(lhs: Expr, rhs: Expr) extends Expr
  case class Sub(lhs: Expr, rhs: Expr) extends Expr

  object Expr {
    val expr1 = Add(Val(3), Val(4))
    val expr2 = Sub(expr1, Val(2))
    val expr3 = Add(Val(3), Sub(Val(2), Val(4)))
    def interpret(e: Expr): Int = e match {
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

  case class ExprAlgebra[T](value: Int => T, add: T => T => T, sub: T => T => T)

  object ExprAlgebra {
    val interpreter = ExprAlgebra[Int](
      value = identity,
      add   = x => y => x + y,
      sub   = x => y => x - y
    )
    val compiler    = ExprAlgebra[List[Instr]](
      value = i => List(PushInstr(i)),
      add   = x => y => x ++ y ++ List(AddInstr),
      sub   = x => y => x ++ y ++ List(SubInstr)
    )
    def fold[T](alg: ExprAlgebra[T])(e: => Expr): T = {
      val ExprAlgebra(value, add, sub) = alg
      e match {
        case Val(i)    => value(i)
        case Add(x, y) => add(fold(alg)(x))(fold(alg)(y))
        case Sub(x, y) => sub(fold(alg)(x))(fold(alg)(y))
      }
    }
    def compile = fold(compiler) _
    def interpret = fold(interpreter) _
  }

  def run() {
    
    import Expr._
    import Instr._
    s"""
      | Expressions:
      | exp1 = $expr1
      | exp2 = $expr2
      | exp3 = $expr3
      | 
      | Direct interpretation:
      | exp1 = ${interpret(expr1)}
      | exp2 = ${interpret(expr2)}
      | exp3 = ${interpret(expr3)}
      | 
      | Direct compilation into instructions:
      | exp1 = ${compile(expr1)}
      | exp2 = ${compile(expr2)}
      | exp3 = ${compile(expr3)}
      | 
      | Algebraic interpretation:
      | exp1 = ${ExprAlgebra.interpret(expr1)}
      | exp2 = ${ExprAlgebra.interpret(expr2)}
      | exp3 = ${ExprAlgebra.interpret(expr3)}
      | 
      | Algebraic compilation into instructions:
      | exp1 = ${ExprAlgebra.compile(expr1)}
      | exp2 = ${ExprAlgebra.compile(expr2)}
      | exp3 = ${ExprAlgebra.compile(expr3)}
    """.stripMargin.split("\n").foreach(println)
  }  
}




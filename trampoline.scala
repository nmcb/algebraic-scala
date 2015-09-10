object trampoline {
    
    // Non-tail recursive ackerman, with inner recursion, no tailrec implementation exists
    def ackermann(m: Int, n: Int): Int = (m, n) match {
        case (0, _)            => n + 1
        case (_, 0) if (m > 0) => ackermann(m - 1, 1)
        case (_, _)            => ackermann(m - 1, ackermann(m, n - 1))  
    }

    // Tail recursive ackerman trampoline chain, inlining possible as we support map and flatMap
    
    // native scala implementation
    import scala.util.control.TailCalls._ 
    def ackermannTailRec(m: Int, n: Int): TailRec[Int] = (m, n) match {
        case (0, _)            => done(n + 1)
        case (_, 0) if (m > 0) => tailcall(ackermannTailRec(m - 1, 1))
        case (_, _)            => for {
            inner <- tailcall(ackermannTailRec(m, n - 1))
            outer <- tailcall(ackermannTailRec(m - 1, inner))
        } yield outer
    }

    // local trampoline implementation
    trait Tramp[+A] {
        @scala.annotation.tailrec
        final def result: A = this match {
            case Done(a)    => a
            case Call(t)    => t().result
            case Cont(a, f) => a match {
                case Done(v)    => f(v).result
                case Call(t)    => t().flatMap(f).result
                case Cont(b, g) => b.flatMap(x => g(x) flatMap f).result
            }
        }

        def flatMap[B](f: A => Tramp[B]): Tramp[B] = this match {
            case Cont(a, g) => Cont(a, (x: Any) => g(x) flatMap f)
            case x          => Cont(x, f)
        }

        def map[B](f: A => B): Tramp[B] = flatMap(a => Done(f(a)))
    }

    case class Done[+A]    (a: A)                          extends Tramp[A]
    case class Call[+A]    (t: () => Tramp[A])             extends Tramp[A]
    case class Cont[A, +B] (s: Tramp[A], f: A => Tramp[B]) extends Tramp[B]
    
    def tdone[A](a: A)           = Done(a)
    def tcall[A](t: => Tramp[A]) = Call(() => t)

    def ackermannTramp(m: Int, n: Int): Tramp[Int] = (m, n) match {
        case (0, _)            => tdone(n + 1)
        case (_, 0) if (m > 0) => tcall(ackermannTramp(m - 1, 1))
        case (_, _)            => for {
            inner <- tcall(ackermannTramp(m, n - 1))
            outer <- tcall(ackermannTramp(m - 1, inner))
        } yield outer
    }
}
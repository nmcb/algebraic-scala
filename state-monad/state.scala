object state {

  // Non-monadic tree labeling:

  sealed trait Tr[A]
  case class Lf[A](a: A) extends Tr[A]
  case class Br[A](l: Tr[A], r: Tr[A]) extends Tr[A]

  val tr1 = Br(Lf('a'), Br(Br(Lf('b'), Lf('a')), Lf('d')))

  type Lt[A] = Tr[(S, A)]
  type S = Int

  def label[A](t: Tr[A]): Lt[A] = {
    def lab(t: Tr[A])(s: S): (S, Lt[A]) = t match {
      case Lf(a)    => ((s + 1), Lf((s, a)))
      case Br(l, r) => { 
        val (s1, ll) = lab(l)(s)
        val (s2, lr) = lab(r)(s1)
        (s2, Br(ll, lr))
      }
    }
    lab(t)(0)._2
  }


  // Monadic tree labeling (idiomatic):

  case class Labeled[A](run: S => (S, A)) {
    import Labeled._
    def flatMap[B](f: A => Labeled[B]): Labeled[B] = Labeled(s => {
      val (s1, a) = run(s)
      f(a).run(s1)
    })
    def map[B](f: A => B) = flatMap(a => unit(f(a)))
  }
  
  object Labeled {
    def unit[A](a: A): Labeled[A] = Labeled(run = s => (s, a))

    def mkm[A](t: Tr[A]): Labeled[Lt[A]] = t match {
      case Lf(a)   => updateState.flatMap(s => unit(Lf((s, a))))
      case Br(l,r) => mkm(l).flatMap(ll => mkm(r).flatMap(lr => unit(Br(ll, lr))))
    }

    val updateState = Labeled[S](s => ((s + 1), s)) 
  }

  def mlabel[A](t: Tr[A]): Lt[A] = Labeled.mkm(t).run(0)._2


  // Monadic tree labeling (haskellian scala):

  type Lab[A] = S => (S, A)

  def hreturn[A](a: A): Lab[A] = (s: S) => (s, a)

  def hbind[A,B](ma: Lab[A], f: A => Lab[B]): Lab[B] = (s: S) => {
    val (s1, a) = ma(s)
    f(a)(s1)
  }

  def hmkm[A](t: Tr[A]): Lab[Lt[A]] = t match {
    case Lf(a)    => hbind(updateLabel, (s: S) => hreturn(Lf((s, a))))
    case Br(l, r) => hbind(hmkm(l), (ll: Lt[A]) => hbind(hmkm(r), (lr: Lt[A]) => hreturn(Br(ll, lr))))
  }

  def updateLabel = (s: S) => ((s + 1), s)

  def hlabel[A](t: Tr[A])(s: S): Lt[A] = hmkm(t)(s)._2
}

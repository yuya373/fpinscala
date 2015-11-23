package fpinscala.laziness

import Stream._

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case empty => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => t().drop(n - 1)
    case Cons(_, t) if n == 1 => t()
    case empty => empty
  }

  // def takeWhile(p: A => Boolean): Stream[A] = this match {
  //   case Cons(h, t) if p(h())  => cons(h(), t().takeWhile(p))
  //   case _ => empty
  // }

  def takeWhile(p: A => Boolean): Stream[A] = {
    foldRight[Stream[A]](empty)((a, b) => if (p(a)) cons(a, b) else empty)
  }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => p(a) && b)
  }

  // def headOption: Option[A] = this match {
  //   case Empty => None
  //   case Cons(h, t) => Some(h())
  // }

  def headOption: Option[A] = {
    foldRight(None: Option[A])((a, b) => Some(a))
  }

  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => List()
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = {
    foldRight(empty: Stream[B])((a, b) => cons(f(a), b))
  }

  def filter(p: A => Boolean): Stream[A] = {
    foldRight(empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else b)
  }

  def append[B >: A](s: Stream[B]): Stream[B] = {
    foldRight(s)((a, b) => cons(a, b))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty: Stream[B])((a, b) => f(a).append(b))
  }

  def map2[B](f: A => B): Stream [B] = {
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }
  }

  def take2(n: Int): Stream[A] = {
    unfold((this, n)) {
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
      case (Cons(h, t), n) if n == 1 => Some((h(), (empty, n - 1)))
      case _ => None
    }
  }

  def takeWhile2(p: A => Boolean): Stream[A] = {
    unfold(this) {
      case Cons(h, t) => {
        if (p(h()))
          Some((h(), t()))
        else
          None
      }
      case _ => None
    }
  }

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] = {
    unfold((this, s)) {
      case (Cons(h, t), Cons(hh, tt)) =>
        Some((f(h(), hh()), (t(), tt())))
      case _ => None
    }
  }

  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] = {
    unfold((this, s)) {
      case (Cons(h, t), Cons(hh, tt)) =>
        Some(((Some(h()), Some(hh())), (t(), tt())))
      case (Cons(h, t), Empty) =>
        Some(((Some(h()), Option.empty[B]), (t(), empty[B])))
      case (Empty, Cons(h, t)) =>
        Some(((Option.empty[A], Some(h())), (empty[A], t())))
      case (Empty, Empty) =>
        None
    }
  }

  def startsWith[B](s: Stream[B]): Boolean = {
    zipAll(s).takeWhile((t) => !t._2.isEmpty).
      forAll { case (a, b) => a == b }
  }

  def tails: Stream[Stream[A]] = {
    unfold(this) {
      case Cons(h, t) => Some((Cons(h, t), t()))
      case _ => None
    } append Stream(empty)
  }

  def hasSubsequence[A](s: Stream[A]): Boolean = {
    tails exists (_ startsWith s)
  }

  def scanRight[B](z: B)(f: (A, B) => B): Stream[B] = {
    foldRight((z, Stream(z)))((a, b) => {
      lazy val b2 = b
      val a2 = f(a, b2._1)
      (a2, cons(a2, b2._2))
    })._2
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = {
    cons(a, constant(a))
  }

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = {
    cons(n, from(n + 1))
  }

  def fibs(): Stream[Int] = {
    def gen(a: Int, b: Int): Stream[Int] = {
      cons(a + b, gen(b, a + b))
    }
    cons(0, gen(0, 1))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h, t)) => cons(h, unfold(t)(f))
      case None => empty
    }

  def constant2[A](a: A): Stream[A] = {
    unfold(a)(_ => Some((a, a)))
  }

  val ones2: Stream[Int] = {
    unfold(1)(_ => Some((1,1)))
  }

  def from2(n: Int): Stream[Int] = {
    // unfold(n)(a => Some((a+1, a+1)))
    unfold(n) { a => Some((a + 1, a + 1)) }
  }

  def fibs2(): Stream[Int] = {
    // unfold((0, 1))(a => Some((a._1 + a._2, (a._2, a._1 + a._2))))
    unfold((0, 1)) { case (a, b) => Some((a, (b, a + b))) }
  }
}

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
    case Cons(h, t) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = {
    @annotation.tailrec
    def loop(s: Stream[A], n: Int): Stream[A] = s match {
      case Empty => Empty
      case Cons(_, t) if n < 1 => s
      case Cons(_, t) => loop(t(), n - 1)
    }
    loop(this, n)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def takeWhileR(p: A => Boolean): Stream[A] = 
    foldRight(empty[A])((a, s) => if(p(a)) cons(a, s) else empty)
 
  @annotation.tailrec
  final def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(h, t) if p(h()) => t() forAll p
    case _ => false
  }

  def forAllR(p: A => Boolean): Boolean = 
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = 
    foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] = 
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else t)

  def append[B >: A](b: => Stream[B]): Stream[B] = 
    foldRight(b)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] = 
    foldRight(empty[B])((a, b) => f(a) append b)

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")

  def toList: List[A] = foldRight(List[A]())((h, t) => h :: t)
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

  val ones: Stream[Int] = Stream.cons(1, ones)
  
  def constant(c: Int): Stream[Int] = cons(c, constant(c))

  def constantL(c: Int): Stream[Int] = {
    lazy val tail: Stream[Int] = Cons(() => c, () => tail)
    tail
  }
  
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def loop(a: Int, b: Int): Stream[Int] = cons(a, loop(b, a + b))
    loop(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty
    case Some((h, t)) => cons(h, unfold(t)(f))
  }
}
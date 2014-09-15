package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  } 
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42 
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101 
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  
  def sum2(ns: List[Int]) = 
    foldRight(ns, 0)((x,y) => x + y)
  
  def product2(ns: List[Double]) = 
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail(Nil)")
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("setHead(Nil, _)")
    case Cons(_, t) => Cons(h, t)
  }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Cons(_, t) if n > 0 => drop(t, n - 1)
    case _ => l
  }

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init(Nil)")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sumLeft(l: List[Int]): Int = foldLeft(l, 0)(_ + _)
  def productLeft(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)
  def lengthLeft[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  def prepend[A](l: List[A], a: A) = Cons(a, l)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])(prepend)

  def foldRightL[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((b, a) => f(a, b))

  def appendR[A](l1: List[A], l2: List[A]): List[A] = 
    foldRightL(l1, l2)(Cons.apply)
  def appendL[A](l1: List[A], l2: List[A]): List[A] =
    reverse(foldLeft(l2, reverse(l1))(prepend))
  
  def concat[A](ls: List[List[A]]): List[A] =
    foldRightL(ls, List[A]())(append)

  def add1(l: List[Int]): List[Int] =
    foldRightL(l, List[Int]())((h, t) => Cons(h + 1, t))

  def toString(l: List[Double]): List[String] =
    foldRightL(l, List[String]())((h, t) => Cons(h.toString, t))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRightL(l, List[B]())((h, t) => Cons(f(h), t))

  def filter[A](l: List[A])(cond: A => Boolean): List[A] =
    foldRightL(l, List[A]())((h, t) => if(cond(h)) Cons(h, t) else t)

  def evens(l: List[Int]): List[Int] = 
    filter(l)(_ % 2 == 0)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = 
    concat(map(l)(f))

  def filterFM[A](l: List[A])(cond: A => Boolean): List[A] = 
    flatMap(l)(a => if(cond(a)) List(a) else Nil)

  def zipAdd(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, zipAdd(t1, t2))
    case _ => Nil
  }

  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    case _ => Nil
  }

  @annotation.tailrec
  def isPrefix[A](l: List[A], pre: List[A]): Boolean = (l, pre) match {
    case (Nil, _) => false
    case (_, Nil) => true
    case (Cons(h1, t1), Cons(h2, t2)) => h1 == h2 && isPrefix(t1, t2)
  }

  @annotation.tailrec
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean =  l match {
      case Cons(_, _) if isPrefix(l, sub) => true
      case Cons(_, t) => hasSubsequence(t, sub)
      case Nil => false
    }
}
package com.fpis.ch3

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val example = Cons(1, Cons(2, Cons(3, Nil)))
  val example2 = List(1, 2, 3)
  val total = sum(example)

  /**
    * EXERCISE 2: Implement the function tail for "removing" the first element
    * of a List. Notice the function takes constant time. What are different choices you
    * could make in your implementation if the List is Nil? We will return to this
    * question in the next chapter.
    *
    * @param as
    * @tparam A
    * @return
    */
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => throw new RuntimeException("Calling tail on Nil")
    case Cons(_, tail) => tail
  }

  /**
    * EXERCISE 3: Generalize tail to the function drop, which removes the first
    * n elements from a list.
    *
    * @param as
    * @tparam A
    * @return
    */
  def drop[A](n: Int, as: List[A]): List[A] = as match {
    case Nil => throw new RuntimeException("Calling drop on Nil")
    case Cons(_, tail) if n == 0 => tail
    case Cons(_, tail) => drop(n - 1, tail)
  }

  /** *
    * EXERCISE 4: Implement dropWhile,10 which removes elements from the
    * List prefix as long as they match a predicate. Again, notice these functions take
    * time proportional only to the number of elements being dropped—we do not need
    * to make a copy of the entire List.
    *
    * @param l
    * @param f
    * @tparam A
    * @return
    */
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => throw new RuntimeException("Calling dropWhile on Nil")
    case Cons(head, tail) if f(head) => dropWhile(tail)(f)
    case Cons(_, tail) => tail
  }

  /**
    * EXERCISE 5: Using the same idea, implement the function setHead for
    * replacing the first element of a List with a different value.
    *
    * @param newHead
    * @param l
    * @tparam A
    * @return
    */
  def setHead[A](newHead: A, l: List[A]): List[A] = l match {
    case Nil => throw new RuntimeException("Calling setHead on Nil")
    case Cons(_, tail) => Cons(newHead, tail)
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  /**
    * EXERCISE 6: Not everything works out so nicely. Implement a function,
    * init, which returns a List consisting of all but the last element of a List. So,
    * given List(1,2,3,4), init will return List(1,2,3). Why can't this
    * function be implemented in constant time like tail?
    *
    * @param l
    * @tparam A
    * @return
    */
  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new RuntimeException("Calling init on Nil")
    case Cons(head, Nil) => Nil
    case Cons(head, tail) => Cons(head, init(tail))
  }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(l: List[Int]) = foldRight(l, 0.0)(_ + _)

  def product2(l: List[Double]) = foldRight(l, 1.0)(_ * _)

  /**
    * EXERCISE 7: Can product implemented using foldRight immediately
    * halt the recursion and return 0.0 if it encounters a 0.0? Why or why not?
    * Consider how any short-circuiting might work if you call foldRight with a
    * large list. This is a deeper question that we'll return to a few chapters from now.
    *
    */

  /**
    * EXERCISE 8: See what happens when you pass Nil and Cons themselves to
    * foldRight, like this: foldRight(List(1,2,3),
    * Nil:List[Int])(Cons(_,_)). What do you think this says about the
    * relationship between foldRight and the data constructors of List?
    * Cons(1, foldRight(Cons(2, Cons(3, Nil)), Nil: List[Int])(Cons(_,_))
    * Cons(1, Cons(2, foldRight(Cons(3, Nil)), Nil: List[Int])(Cons(_,_))
    * Cons(1, Cons(2, Cons(3, Nil)))
    *
    */

  /**
    * EXERCISE 9: Compute the length of a list using foldRight.
    * @param l
    * @tparam A
    * @return
    */
  def length[A](l: List[A]): Int = ???

  /**
    * EXERCISE 10: foldRight is not tail-recursive and will StackOverflow
    * for large lists. Convince yourself that this is the case, then write another general
    * list-recursion function, foldLeft that is tail-recursive, using the techniques we
    * discussed in the previous chapter. Here is its signature:
    *
    * @param l
    * @param z
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = ???

  /**
    * main method
    * @param args
    */
  def main(args: Array[String]): Unit = {
    val folded = foldRight(List(1,2,3), Nil: List[Int])(Cons(_, _))
    println(folded)
  }


}

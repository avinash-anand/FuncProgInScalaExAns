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
  def drop[A](n: Int, as: List[A]): List[A] = ???

  val example = Cons(1, Cons(2, Cons(3, Nil)))
  val example2 = List(1, 2, 3)
  val total = sum(example)
}

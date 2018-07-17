package com.fpis.ch03

/**
  * EXERCISE 1: What will the result of the following match expression be?
  */
object Ex1 {

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
  }

  import List.sum

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x1, Cons(2, Cons(4, _))) => x1
    case Nil => 42
    case Cons(x1, Cons(y1, Cons(3, Cons(4, _)))) => x1 + y1
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def main(args: Array[String]): Unit = {
    println(x)
  }

}

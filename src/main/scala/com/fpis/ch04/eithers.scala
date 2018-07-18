package com.fpis.ch04

/**
  * EXERCISE 7: Implement versions of map, flatMap, orElse, and map2 on
  * Either that operate on the Right value.
  *
  * @tparam E
  * @tparam A
  */
sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = ???

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = ???

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = ???

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = ???

}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Main {
  def main(args: Array[String]): Unit = {

  }
}

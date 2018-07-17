package com.fpis.ch4

/**
  * EXERCISE 1: We'll explore when you'd use each of these next. But first, as an
  * exercise, implement all of the above functions on Option. As you implement
  * each function, try to think about what it means and in what situations you'd use it.
  * Here are a few hints:
  * - It is fine to use pattern matching, though you should be able to implement all the
  * functions besides map and getOrElse without resorting to pattern matching.
  * - For map and flatMap, the type signature should be sufficient to determine the
  * implementation.
  * - getOrElse returns the result inside the Some case of the Option, or if the Option is None,
  * returns the given default value.
  * - orElse returns the first Option if it is defined, otherwise, returns the second Option.
  *
  * @tparam A
  */
sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case None => None
  }

  def flatMap2[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(a) => Some(a)
    case None => ob
  }

  def orElse2[B >: A](ob: => Option[B]): Option[B] = this map (Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => Some(a)
    case _ => None
  }

  def filter2(f: A => Boolean): Option[A] = this flatMap (a => if (f(a)) Some(a) else None)

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

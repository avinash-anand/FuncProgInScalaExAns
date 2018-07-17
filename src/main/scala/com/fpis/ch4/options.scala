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

object Ex_2 {

  def main(args: Array[String]): Unit = {
    val s1 = Seq(1.0, 2, 3, 4, 5)
    println(mean(s1))
    println(variance(s1))
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  /**
    * EXERCISE 2: Implement the variance function (if the mean is m, variance
    * is the mean of math.pow(x - m, 2), see definition) in terms of mean and
    * flatMap.3
    * Footnote 3mVariance can actually be computed in one pass,
    * but for pedagogical purposes we will compute it
    * using two passes. The first will compute the mean of the data set,
    * and the second will compute the mean squared
    * difference from this mean.
    *
    * @param xs
    * @return
    */
  def variance(xs: Seq[Double]): Option[Double] = {
    val meanFound: Option[Double] = mean(xs)
    meanFound match {
      case Some(m) => mean(xs.map(a => Math.pow(a - m, 2)))
      case None => None
    }
  }

}

object Ex_3 {

  import java.util.regex._

  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }

  def mkMatcher(pat: String): Option[String => Boolean] =
    pattern(pat) map (p => (s: String) => p.matcher(s).matches)

  def mkMatcher_1(pat: String): Option[String => Boolean] =
    for {
      p <- pattern(pat)
    } yield (s: String) => p.matcher(s).matches

  def doesMatch(pat: String, s: String): Option[Boolean] =
    for {
      p <- mkMatcher_1(pat)
    } yield p(s)

  def bothMatch(pat: String, pat2: String, s: String): Option[Boolean] =
    for {
      f <- mkMatcher(pat)
      g <- mkMatcher(pat2)
    } yield f(s) && g(s)

  def bothMatch_1(pat: String, pat2: String, s: String): Option[Boolean] =
    mkMatcher(pat) flatMap (f =>
      mkMatcher(pat2) map (g =>
        f(s) && g(s)))

  /**
    * EXERCISE 3: is an instance of a more general bothMatch pattern. Write a
    * generic function map2, that combines two Option values using a binary function.
    * If either Option value is None, then the return value is too. Here is its signature:
    *
    * @param a
    * @param b
    * @param f
    * @tparam A
    * @tparam B
    * @tparam C
    * @return
    */
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (a1 => b.map(b1 => f(a1, b1)))

  /**
    * EXERCISE 4: Re-implement bothMatch above in terms of this new function,
    * to the extent possible.
    *
    * @param pat1
    * @param pat2
    * @param s
    * @return
    */
  def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] =
    map2(pattern(pat1), pattern(pat2)) { (p1, p2) =>
      p1.matcher(s).matches() && p2.matcher(s).matches()
    }

}

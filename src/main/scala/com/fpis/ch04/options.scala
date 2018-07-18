package com.fpis.ch04

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

object Ex_3_4_5_6 {

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

  /**
    * EXERCISE 5: Write a function sequence, that combines a list of Options
    * into one option containing a list of all the Some values in the original list. If the
    * original list contains None even once, the result of the function should be None,
    * otherwise the result should be Some with a list of all the values. Here is its
    * signature:
    *
    * @param a
    * @tparam A
    * @return
    */
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case head :: tail => head.flatMap(h1 => sequence(tail) map (l => h1 :: l))
  }

  def sequenceViaFoldRight[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight(Some(Nil): Option[List[A]])((optA, b) => map2(optA, b)((a, b) => a :: b))

  /** Wanting to sequence the results of a map this way is a common enough occurrence to
    * warrant a new generic function traverse, with the following signature:
    * {{{def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]]}}}
    * EXERCISE 6: Implement this function. It is straightforward to do using map
    * and sequence, but try for a more efficient implementation that only looks at the
    * list once. In fact, implement sequence in terms of traverse.
    *
    * @param a
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case head :: tail => map2(f(head), traverse(tail)(f))((a1, b1) => a1 :: b1)
  }

  def traverseViaMapAndSequence[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    sequence(a map f)

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(a1 => a1)

  def main(args: Array[String]): Unit = {
    println(sequence(List(Some(1), Some(2), Some(3))))
    println(sequence(List(Some(1), Some(2), None)))
    println(sequenceViaFoldRight(List(Some(1), Some(2), Some(3))))
    println(sequenceViaFoldRight(List(Some(1), Some(2), None)))
    println(traverse(List(Some(1), Some(2), Some(3)))(a1 => a1))
    println(traverseViaMapAndSequence(List(Some(1), Some(2), None))(a1 => a1))
    println(sequence2(List(Some(1), Some(2), Some(3))))
    println(sequence2(List(Some(1), Some(2), None)))
  }

}

package com.fpis.ch03

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
    case Cons(_, Nil) => Nil
    case Cons(head, tail) => Cons(head, init(tail))
  }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(l: List[Int]): Int = foldRight(l, 0)(_ + _)

  def product2(l: List[Double]): Double = foldRight(l, 1.0)(_ * _)

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
    *
    * @param l
    * @tparam A
    * @return
    */
  def length[A](l: List[A]): Int = foldRight(l, 0)((a, b) => b + 1)

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
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
  }


  /**
    * EXERCISE 11: Write sum, product, and a function to compute the length of
    * a list using foldLeft.
    *
    * @param l
    * @return
    */
  def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def product3(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int = foldLeft(l, 0)((b, a) => b + 1)

  /**
    * EXERCISE 12: Write a function that returns the reverse of a list (so given
    * List(1,2,3) it returns List(3,2,1)). See if you can write it using a fold.
    *
    * @param l
    * @tparam A
    * @return
    */
  def reverse[A](l: List[A]): List[A] = {
    def loop(l: List[A], acc: List[A]): List[A] = l match {
      case Nil => acc
      case Cons(head, tail) => loop(tail, Cons(head, acc))
    }

    loop(l, Nil)
  }

  def reverseViaFoldLeft[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((b, a) => Cons(a, b))

  /**
    * EXERCISE 13 (hard): Can you write foldLeft in terms of foldRight?
    * How about the other way around?
    *
    * @param l
    * @param z
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B = foldRight(l, z)((a, b) => f(b, a))

  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = foldLeft(l, z)((b, a) => f(a, b))

  /**
    * EXERCISE 14: Implement append in terms of either foldLeft or
    * foldRight.
    *
    * @param a1
    * @param a2
    * @tparam A
    * @return
    */
  def appendViaFoldLeft[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(reverseViaFoldLeft(a1), a2)((b, a) => Cons(a, b))

  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((a, b) => Cons(a, b))

  /**
    * EXERCISE 15 (hard): Write a function that concatenates a list of lists into a
    * single list. Its runtime should be linear in the total length of all lists. Try to use
    * functions we have already defined.
    *
    * @param l
    * @tparam A
    * @return
    */
  def concatenateLists[A](l: List[List[A]]): List[A] =
    foldLeft(l, Nil: List[A])((b, a) => appendViaFoldLeft(b, a))

  /**
    * EXERCISE 16: Write a function that transforms a list of integers by adding 1
    * to each element. (Reminder: this should be a pure function that returns a new
    * List!)
    *
    * @param l
    * @return
    */
  def transformByAdding1(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(head, tail) => Cons(head + 1, transformByAdding1(tail))
  }

  /**
    * EXERCISE 17: Write a function that turns each value in a List[Double]
    * into a String.
    *
    * @param l
    * @return
    */
  def mkStringListOfDouble(l: List[Double]): String = l match {
    case Nil => " "
    case Cons(head, tail) => s"$head " + mkStringListOfDouble(tail)
  }

  /**
    * EXERCISE 18: Write a function map, that generalizes modifying each element
    * in a list while maintaining the structure of the list. Here is its signature:
    *
    * @param l
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  def map[A, B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(head, tail) => Cons(f(head), map(tail)(f))
  }

  /**
    * EXERCISE 19: Write a function filter that removes elements from a list
    * unless they satisfy a given predicate. Use it to remote all odd numbers from a
    * List[Int].
    *
    * @param l
    * @param f
    * @tparam A
    * @return
    */
  def filter[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(head, tail) if f(head) => Cons(head, filter(tail)(f))
    case Cons(_, tail) => filter(tail)(f)
  }

  def filterOdd(l: List[Int]): List[Int] = filter(l)(_ % 2 == 0)

  /**
    * EXERCISE 20: Write a function flatMap, that works like map except that
    * the function given will return a list instead of a single result, and that list should be
    * inserted into the final resulting list. Here is its signature:
    *
    * For instance flatMap(List(1,2,3))(i => List(i,i)) should
    * result in List(1,1,2,2,3,3).
    *
    * @param l
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = l match {
    case Nil => Nil
    case Cons(head, tail) => append(f(head), flatMap(tail)(f))
  }

  /**
    * EXERCISE 21: Can you use flatMap to implement filter?
    *
    * @param l
    * @param f
    * @tparam A
    * @return
    */
  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l) { a =>
    if (f(a)) Cons(a, Nil)
    else Nil
  }

  /**
    * EXERCISE 22: Write a function that accepts two lists and constructs a new list
    * by adding corresponding elements. For example, List(1,2,3) and
    * List(4,5,6) becomes List(5,7,9).
    *
    * @param l1
    * @param l2
    * @return
    */
  def sumOf2List(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Nil, Nil) => Nil
    case (x1: Cons[Int], Nil) => x1
    case (Nil, y1: Cons[Int]) => y1
    case (Cons(head1, tail1), Cons(head2, tail2)) => Cons(head1 + head2, sumOf2List(tail1, tail2))
  }

  /**
    * EXERCISE 23: Generalize the function you just wrote so that it's not specific to
    * integers or addition.
    *
    * @param l1
    * @param l2
    * @param z
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  def mapOn2SimilarList[A, B](l1: List[A], l2: List[A], z: A)(f: (A, A) => B): List[B] = (l1, l2) match {
    case (Nil, Nil) => Nil
    case (Nil, Cons(head, tail)) => Cons(f(z, head), mapOn2SimilarList(Nil: List[A], tail, z)(f))
    case (Cons(head, tail), Nil) => Cons(f(head, z), mapOn2SimilarList(tail, Nil: List[A], z)(f))
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), mapOn2SimilarList(t1, t2, z)(f))
  }

  def mapOn2List[A, B, C](l1: List[A], l2: List[B], z1: A, z2: B)(f: (A, B) => C): List[C] = (l1, l2) match {
    case (Nil, Nil) => Nil: List[C]
    case (Nil, Cons(head, tail)) => Cons(f(z1, head), mapOn2List(Nil: List[A], tail, z1, z2)(f))
    case (Cons(head, tail), Nil) => Cons(f(head, z2), mapOn2List(tail, Nil: List[B], z1, z2)(f))
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), mapOn2List(t1, t2, z1, z2)(f))
  }

  //maybe above question says about zip, so let's write zip
  def zip[A, B](l1: List[A], l2: List[B]): List[(A, B)] = (l1, l2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))
  }

  def zipAddInt(l1: List[Int], l2: List[Int]): List[Int] = {
    map(zip(l1, l2))(a => a._1 + a._2)
  }

  def zipMap[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = {
    map(zip(l1, l2))(zipped => f(zipped._1, zipped._2))
  }

  /**
    * EXERCISE 24 (hard): As an example, implement hasSubsequence for
    * checking whether a List contains another List as a subsequence. For instance,
    * List(1,2,3,4) would have List(1,2), List(2,3), and List(4) as
    * subsequences, among others. You may have some difficulty finding a concise
    * purely functional implementation that is also efficient. That's okay. Implement the
    * function however comes most naturally. We will return to this implementation in a
    * couple of chapters and hopefully improve on it. Note: any two values, x, and y,
    * can be compared for equality in Scala using the expression x == y.
    *
    * @param l
    * @param sub
    * @tparam A
    * @return
    */
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = l match {
    case Nil => sub == Nil
    case _ if startsWith(l, sub) => true
    case Cons(_, lTail) => hasSubsequence(lTail, sub)
  }

  def startsWith[A](l: List[A], startList: List[A]): Boolean = (l, startList) match {
    case (_, Nil) => true
    case (Cons(lHead, lTail), Cons(startHead, startTail)) if lHead == startHead => startsWith(lTail, startTail)
    case _ => false
  }

  /**
    * main method
    *
    * @param args
    */
  def main(args: Array[String]): Unit = {
    println(length(example))
    println(reverse(example))
    val e1 = List(1, 2, 3, 4)
    val e2 = List(5, 6, 7, 8, 9)
    val e3 = List(10, 11, 12)
    val e4 = Nil: List[Int]
    println(appendViaFoldLeft(e1, e2))
    println(appendViaFoldRight(e1, e2))
    println(concatenateLists(List(e1, e2, e3, e4)))
    println(transformByAdding1(e1))
    println(mkStringListOfDouble(List(1.0, 2.0, 3.0)))
    println(map(e1)(a => a + 1))
    println(filter(e1)(_ > 2))
    println(filterOdd(e1))
    println(flatMap(List(1, 2, 3))(i => List(i, i)))
    println(filterViaFlatMap(List(1, 2, 3, 4))(_ % 2 == 0))
    println(sumOf2List(List(1, 2, 3), List(4, 5, 6)))
    println(sumOf2List(List(1, 2), List(4, 5, 6)))
    println(zip(List(1, 2, 3), List(4, 5, 6)))
    println(zipAddInt(List(1, 2, 3), List(4, 5, 6)))
    println(zipMap(List(1, 2, 3), List(4, 5, 6))((a, b) => a + b))
    println(hasSubsequence(List(1, 2, 3, 4), List(1, 2)))
    println(hasSubsequence(List(1, 2, 3, 4), List(2, 3)))
    println(hasSubsequence(List(1, 2, 3, 4), List(3, 4)))
    println(hasSubsequence(List(1, 2, 3, 4), List(4)))
    println(hasSubsequence(List(1, 2, 3, 4), List(3)))
    println(hasSubsequence(List(1, 2, 3, 4), List(1, 3)))
  }


}

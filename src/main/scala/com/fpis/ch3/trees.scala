package com.fpis.ch3

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  /**
    * EXERCISE 25: Write a function size that counts the number of nodes in a tree.
    *
    * @param t
    * @tparam A
    * @return
    */
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => size(left) + size(right)
  }

  /**
    * EXERCISE 26: Write a function maximum that returns the maximum element
    * in a Tree[Int]. (Note: in Scala, you can use x.max(y) or x max y to
    * compute the maximum of two integers x and y.)
    *
    * @param t
    * @return
    */
  def maximum(t: Tree[Int]): Int = {

    def loop(t: Tree[Int], maxVal: Int): Int = t match {
      case Leaf(value) => maxVal max value
      case Branch(left, right) => loop(left, maxVal) max loop(right, maxVal)
    }

    loop(t, Int.MinValue)

  }

  /**
    * EXERCISE 27: Write a function depth that returns the maximum path length
    * from the root of a tree to any leaf.
    *
    * @param t
    * @tparam A
    * @return
    */
  def depth[A](t: Tree[A]): Int = {

    def loop[A](t: Tree[A], depth: Int): Int = t match {
      case Leaf(_) => depth + 1
      case Branch(left, right) => loop(left, depth + 1) max loop(right, depth + 1)
    }

    loop(t, 0)
  }

  /**
    * EXERCISE 28: Write a function map, analogous to the method of the same
    * name on List, that modifies each element in a tree with a given function.
    *
    * @param t
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  def map[A,B](t: Tree[A])(f: A => B) : Tree[B] = t match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }


  def main(args: Array[String]): Unit = {

    val tree1 = Branch(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4))), Leaf(5))
    println(size(tree1))
    println(maximum(tree1))
    println(depth(tree1))
    println(map(tree1)(a => a + 1))

  }


}

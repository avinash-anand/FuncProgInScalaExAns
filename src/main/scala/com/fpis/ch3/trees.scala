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
  def size[A](t: Tree[A]): Int = ???


  def main(args: Array[String]): Unit = {

  }


}

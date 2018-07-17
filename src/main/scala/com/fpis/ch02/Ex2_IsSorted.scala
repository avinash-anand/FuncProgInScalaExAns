package com.fpis.ch02

import scala.annotation.tailrec

/**
  * EXERCISE 2: Implement isSorted,
  * which checks whether an Array[A] is sorted according to a given comparison function.
  */
object Ex2_IsSorted {

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(arr: Array[A], gt: (A, A) => Boolean, currentState: Boolean): Boolean = {
      if (arr.length == 0 || arr.length == 1) currentState
      else if (gt(arr.head, arr.tail.head)) loop(arr.tail, gt, currentState = true)
      else false
    }

    if (as.length == 0 || as.length == 1) true
    else loop(as, gt, currentState = false)
  }


  def gtIntAsc: (Int, Int) => Boolean = (a: Int, b: Int) => a < b

  def gtIntDesc: (Int, Int) => Boolean = (a: Int, b: Int) => a > b

  def main(args: Array[String]): Unit = {
    val arr1 = Array(1, 2, 3, 4, 5)
    println(isSorted(arr1, gtIntAsc))
    println(isSorted(arr1.reverse, gtIntDesc))
  }

}

package com.fpis.ch2

/**
 * EXERCISE 2: Implement isSorted,
 * which checks whether an Array[A] is sorted according to a given comparison function.
 */
object Ex2_IsSorted {

  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    
    false
  }

  def gt[A](first:A, second:A) : Boolean = {
    first.toString > second.toString
  }

}

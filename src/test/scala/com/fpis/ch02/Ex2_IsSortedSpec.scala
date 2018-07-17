package com.fpis.ch02

import org.scalatest.{FlatSpec, Matchers}

class Ex2_IsSortedSpec extends FlatSpec with Matchers {

  val arr1 = Array(1, 2, 3, 4, 5)

  def gtIntAsc: (Int, Int) => Boolean = (a: Int, b: Int) => a < b

  def gtIntDesc: (Int, Int) => Boolean = (a: Int, b: Int) => a > b

  "Ex2_IsSorted - isSorted method" should "return true for sorted int array" in {
    assert(Ex2_IsSorted.isSorted(arr1, gtIntAsc))
  }

  it should "return true for sorted int array in reverse direction" in {
    assert(Ex2_IsSorted.isSorted(arr1.reverse, gtIntDesc))
  }

  it should "return false for reverse-sorted int array" in {
    assert(!Ex2_IsSorted.isSorted(arr1, gtIntDesc))
  }

}

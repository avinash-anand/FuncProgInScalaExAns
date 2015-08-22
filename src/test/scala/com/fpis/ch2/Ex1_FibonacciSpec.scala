package com.fpis.ch2

import org.scalatest.{FlatSpec, Matchers}

class Ex1_FibonacciSpec extends FlatSpec with Matchers {

  "Ex1_Fibonacci - fib method" should "for n<0, throw exception" in {
    intercept[RuntimeException] {
      val result = Ex1_Fibonacci.fib(-1)
    }
  }

  it should "for n=0, return 0" in {
    val result = Ex1_Fibonacci.fib(0)
    result shouldBe (0)
  }

  it should "for n=1, return 1" in {
    val result = Ex1_Fibonacci.fib(1)
    result shouldBe (1)
  }

  it should "for n=2, return 2" in {
    val result = Ex1_Fibonacci.fib(2)
    result shouldBe (2)
  }

  it should "for n=3, return 3" in {
    val result = Ex1_Fibonacci.fib(3)
    result shouldBe (3)
  }

  it should "for n=4, return 5" in {
    val result = Ex1_Fibonacci.fib(4)
    result shouldBe (5)
  }

  it should "for n=5, return 8" in {
    val result = Ex1_Fibonacci.fib(5)
    result shouldBe (8)
  }

  it should "for n=6, return 13" in {
    val result = Ex1_Fibonacci.fib(6)
    result shouldBe (13)
  }

  it should "for n= 7, return 21" in {
    val result = Ex1_Fibonacci.fib(7)
    result shouldBe (21)
  }

  it should "for n=8, return 34" in {
    val result = Ex1_Fibonacci.fib(8)
    result shouldBe (34)
  }

  it should "for n=9, return 55" in {
    val result = Ex1_Fibonacci.fib(9)
    result shouldBe (55)
  }

  it should "for n=10, return 89" in {
    val result = Ex1_Fibonacci.fib(10)
//    println(s"### 10 $result")
    result shouldBe (89)
  }

}

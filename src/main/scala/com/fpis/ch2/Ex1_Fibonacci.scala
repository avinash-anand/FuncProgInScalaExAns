package com.fpis.ch2

import scala.annotation.tailrec

object Ex1_Fibonacci {

  def fib(n: Int): Int = {

    @tailrec
    def go(prev: Int, current: Int, n: Int): Int = {
      if(n==0) current
      else {
        go(current, prev+current, n-1)
      }
    }

    if(n <0) throw new RuntimeException("Fibonacci not defined for negative values")
    else if(n==0) 0
    else go(0,1,n)
  }

}

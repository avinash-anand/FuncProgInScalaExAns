package com.fpis.ch02

/**
  * EXERCISE 6: Implement the higher-order function that composes two
  * functions.
  */

object Ex6_Compose {

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    def fn_from_A_To_C: A => C = (a: A) => f(g(a))
    // or
    // def fn(a: A): C = f(g(a))
    fn_from_A_To_C
  }

}

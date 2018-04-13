package com.fpis.ch2

/**
  * EXERCISE 5 (optional): Implement uncurry, which reverses the
  * transformation of curry. Note that since => associates to the right,
  * A => (B => C) can be written as A => B => C.
  */

object Ex5_Uncurry {

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    def fn(a: A, b: B): C = f(a)(b)

    fn
  }

}

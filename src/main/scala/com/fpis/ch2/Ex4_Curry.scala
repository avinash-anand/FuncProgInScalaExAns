package com.fpis.ch2

/**
  * EXERCISE 4 (hard): Let's look at another example, currying, which converts a
  * function of N arguments into a function of one argument that returns another
  * function as its result.11 Here again, there is only one implementation that
  * typechecks.
  */

object Ex4_Curry {

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    //cheating would be this line
    //def fn(a: A): B => C = f(a, _)

    def f1(a: A): B => C = {
      Ex3_Partial1.partial1(a, f)
    }

    f1
  }

}

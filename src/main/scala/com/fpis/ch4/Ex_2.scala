package com.fpis.ch4

object Ex_2 {

  def main(args: Array[String]): Unit = {
    val s1 = Seq(1.0, 2, 3, 4, 5)
    println(mean(s1))
    println(variance(s1))
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  /**
    * EXERCISE 2: Implement the variance function (if the mean is m, variance
    * is the mean of math.pow(x - m, 2), see definition) in terms of mean and
    * flatMap.3
    * Footnote 3mVariance can actually be computed in one pass,
    * but for pedagogical purposes we will compute it
    * using two passes. The first will compute the mean of the data set,
    * and the second will compute the mean squared
    * difference from this mean.
    *
    * @param xs
    * @return
    */
  def variance(xs: Seq[Double]): Option[Double] = {
    val meanFound: Option[Double] = mean(xs)
    meanFound match {
      case Some(m) => mean(xs.map(a => Math.pow(a - m, 2)))
      case None => None
    }
  }

}

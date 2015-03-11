package problems

import math.pow

/**
 * http://projecteuler.net/problem=30
 *
 */
object Problem_0030 extends Problem {

  val powers: Map[Int, Int] = (0 to 9).map { n => n -> pow(n, 5).toInt }.toMap
  val maxDigitValue = powers(9)
  val maxDigits = (1 to 10 find (d => maxDigitValue * d < pow(10, d))).get
  val maxValue = maxDigitValue * maxDigits

  def toPowers(n: Int): Int = {
    (toDigits(n) map powers).sum
  }

  def toDigits(n: Int): List[Int] = {
    if (n == 0) List(0)
    else {
      (Stream.iterate(n)(_ / 10) takeWhile (_ != 0) map (_ % 10)).toList.reverse
    }
  }

  def answer = {
    val nums = for {
      n <- 10 to maxValue
      if n == toPowers(n)
    } yield n

    nums.sum
  }

}
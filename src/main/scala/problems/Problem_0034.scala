package problems

import problems.Problem_0030.toDigits

import scala.math._

/**
 * http://projecteuler.net/problem=34
 *
 * Find the sum of all numbers which are equal to the sum of the factorial of their digits.
 */
object Problem_0034 extends Problem {

  val factorials = (0 to 9 map (n => n -> factorial(n))).toMap
  val maxDigitValue = factorial(9)
  val maxDigits = (1 to 10 find (d => maxDigitValue * d < pow(10, d))).get
  val maxValue = maxDigitValue * maxDigits

  def factorial(n: Int): Int = {
    if (n == 0) 1
    else (1 to abs(n)).product
  }

  def factorialSum(n: Int): Int = (toDigits(n) map factorial).sum


  def answer = {
    val nums = for {
      n <- 10 to maxValue
      if n == factorialSum(n)
    } yield n

    nums.sum
  }
}
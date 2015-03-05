package problems

import java.lang.Math._

/**
 * http://projecteuler.net/problem=23
 *
 * Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.
 */
object Problem_0023 extends Problem {

  val limit = 28123

  def getDivisorSum(n: Int): Int = {
    val divisors: Seq[Int] = (1 to sqrt(n).toInt).filter{ n % _ == 0 }
    (divisors ++ (divisors drop 1 map(n / _))).toSet.sum
  }

  def isAbundant(n: Int): Boolean = getDivisorSum(n) > n

  lazy val abundantNumbers = for {
      n <- 12 to limit
      if isAbundant(n)
    } yield n

  def answer = {
    val sums = (for {
      a <- abundantNumbers
      b <- abundantNumbers
    } yield a + b).toSet

    (1 to limit filterNot sums.contains).sum
  }

}
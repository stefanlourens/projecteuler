package problems

import problems.Problem_0021.getDivisorSum

/**
 * http://projecteuler.net/problem=23
 *
 * Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.
 */
object Problem_0023 extends Problem {

  val limit = 28123
  def isAbundant(n: Int): Boolean = getDivisorSum(n) > n

  lazy val abundantNumbers = for {
      n <- 12 to limit
      if isAbundant(n)
    } yield n

  def answer = {
    val sums = for {
      a <- abundantNumbers
      b <- abundantNumbers
    } yield a + b

    println(sums.length)

    (1 to limit filterNot(sums.contains(_))).sum
  }

}
package problems

import java.lang.Math.sqrt

/**
 * http://projecteuler.net/problem=21
 *
 * Evaluate the sum of all the amicable numbers under 10000.
 */
object Problem_0021 extends Problem {

  def getDivisorSum(n: Int): Int = {
    val divisors: Seq[Int] = (1 to sqrt(n).toInt).filter{ n % _ == 0 }
    (divisors ++ (divisors drop 1 map(n / _))).toSet.sum
  }

  def answer = {
    val nums = {
      for {
        n <- 1 to 10000
      } yield n -> getDivisorSum(n)
    }.toMap.withDefaultValue(0)

    (nums filter { case (n, divSum) => n != divSum && nums(divSum) == n }).keys.sum
  }
}
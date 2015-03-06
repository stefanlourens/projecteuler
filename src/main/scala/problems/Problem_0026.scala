package problems


import Math.max

/**
 * http://projecteuler.net/problem=26
 *
 * Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.
 */
object Problem_0026 extends Problem {

  /**
   * The period of 1/k for integer k is always ≤ k − 1.
   *
   * If λ is the smallest power of 10 that leaves a remainder of 1 when divided by n,
   * then there are λ digits in the period of 1/n:
   * λ is the smallest number for which 10^λ mod n = 1
   * @param denominator 1/denominator
   * @return
   */
  def findPeriod(denominator: Int): Option[Int] = {
    val base = BigDecimal(10)
    val one = BigDecimal(1)

     1 to denominator find(base.pow(_) % BigDecimal(denominator) == one)
  }

  def answer = {
     (1 to 1000 map { n => (n, findPeriod(n))} maxBy { case (n, period) => period.getOrElse(0)})._1
  }

}
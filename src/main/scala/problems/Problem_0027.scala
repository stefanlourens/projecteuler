package problems

import java.lang.Math.pow

/**
 * http://projecteuler.net/problem=27
 *
 * Find the product of the coefficients, a and b, for the quadratic expression
 * that produces the maximum number of primes for consecutive values of n, starting with n = 0.
 */
object Problem_0027 extends Problem {

  val primes = Problem_0010.primes

  def isPrime(n: Long): Boolean = {
    primes takeWhile (_ <= n) contains n
  }

  /**
   * n² + an + b, where |a| < 1000 and |b| < 1000
   * @param a
   * @param b
   * @return
   */
  def formulaPrimeCount(a: Int, b: Int): Int = {
    (0 to Int.MaxValue takeWhile { n => isPrime(pow(n, 2).toLong + (n * a) + b) } ).size
  }

  def answer = {
    val results = for {
      a <- -999 to 999
      b <- -999 to 999
    } yield (a, b, formulaPrimeCount(a, b))

    println(results.size + "total")
    val (a, b, size) = results.maxBy { case (_, _, count) => count }
    println("size = " + size)
    println("a = " + a)
    println("b = " + b)
    a * b
  }

}
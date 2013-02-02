package problems

import Problem_0003.isPrime

/**
 * http://projecteuler.net/problem=37
 *
 * Find the sum of the only eleven primes that are both truncatable from left to right and right to left.
 */
object Problem_0037 extends Problem {

  def isTruncatablePrime(n: Long): Boolean = {
    if (!isPrime(n)) false
    else {
      val str = n.toString
      val truncated = for {
        i <- 1 until str.length
      } yield List(str.drop(i), str.dropRight(i))

      truncated.flatten.par.forall { n => isPrime(n.toInt) }
    }
  }

  val truncatablePrimes = for {
    n <- (9 to Int.MaxValue by 2).toStream
    if (isTruncatablePrime(n))
  } yield n

  def answer = truncatablePrimes take 11 sum
}
package problems

import math.sqrt

/**
 * The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
 *
 * Find the sum of all the primes below two million.
 */
object Problem_0010 extends Problem {

  lazy val primes: Stream[Long] = 2l #:: {
    for {
      n <- (3l to Int.MaxValue by 2l).toStream
      maxFactor = sqrt(n)
      if primes.takeWhile(_ <= maxFactor.intValue()).forall(n % _ != 0)
    } yield n
  }

  def answer = {
    primes.takeWhile{ _ < 2000000 }.sum
  }
}
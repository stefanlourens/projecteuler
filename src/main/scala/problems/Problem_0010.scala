package problems

import Problem_0003.isPrime

/**
 * The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
 *
 * Find the sum of all the primes below two million.
 */
object Problem_0010 extends Problem {

  val primes = for {
    n <- 2l #:: (3l to Int.MaxValue by 2l).toStream
    if isPrime(n)
  } yield n



  def answer = {
    primes.takeWhile{ _ < 2000000 }.sum
  }
}
package problems

import Problem_0003.isPrime

/**
 * By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
 *
 * What is the 10 001st prime number?
 *
 */
object Problem_0007 extends Problem {

  def answer = {
    val primes = for {
      n <- 1 #:: 2 #:: (3 to Int.MaxValue by 2).toStream
      if (isPrime(n))
    } yield n

    primes(10000)
  }

}
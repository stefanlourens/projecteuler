package problems

import Problem_0003.isPrime

/**
 * The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
 *
 * Find the sum of all the primes below two million.
 */
object Problem_0010 extends Problem {

  def answer = {
    for {
      n <- 2 #:: (3 until 10 by 2 toStream)
      if (isPrime(n))
    } yield n
  } toList

}
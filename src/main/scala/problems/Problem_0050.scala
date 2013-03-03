package problems

import math.{floor, sqrt}
import Problem_0003.isPrime

/**
 * http://projecteuler.net/problem=50
 * 
 * Which prime, below one-million, can be written as the sum of the most consecutive primes?
 */
object Problem_0050 extends Problem {

  def primesFrom(n: Int) = for {
    n <- (n to Int.MaxValue by 2).toStream
    if (isPrime(n))
  } yield n

  val fourDigitPrimes = primesFrom(1485).takeWhile { _ < 10000 }
  
    def isPrime(n: Long) = {
    val primeCache = Set[Long]()
    val maxFactor = floor(sqrt(n)).toInt

    if (primeCache.contains(n)) true
    else if (n <= 1) false
    else if (n == 2) true
    else {
      if (2 #:: (3 to maxFactor by 2 toStream) forall (n % _ != 0)) {
        primeCache + n
        true
      } else false
    }
  }

  val primes = for {
    n <- 2 #:: (3 to Int.MaxValue by 2).toStream
    if (isPrime(n))
  } yield n

  //Candidate sums
  val primeCandidates = primes dropWhile { _ < 1000 } takeWhile { _ < 1000000 }

  primeCandidates map {
    p =>
      {
        (p, primes takeWhile { _ < p })
      }
  }

  primes slice (3, 24) toList

  isPrime(953L)
  sqrt(953)
  
  
  //@TODO : Find numbers with commons difference

  def answer = ???
  
}
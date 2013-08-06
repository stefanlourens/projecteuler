package problems

import math.{ floor, sqrt }

/**
 * The prime factors of 13195 are 5, 7, 13 and 29. What is the largest prime
 * factor of the number 600851475143 ?
 */
object Problem_0003 extends Problem {

  def isPrime(n: Long): Boolean = n match {
    case 1 => false
    case 2 => true
    case _ => {
      lazy val maxFactor = floor(sqrt(n)).toInt
      2 #:: (3 to maxFactor by 2 toStream) forall (n % _ != 0)
    }
  }

  def answer = {
    val n = 600851475143l
    lazy val maxFactor = floor(sqrt(n)).toInt

    (2 :: (3 to maxFactor by 2 toList) filter (n % _ == 0) filter (isPrime(_))).max
  }

}
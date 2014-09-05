package problems

import Problem_0003.isPrime

/**
 * http://projecteuler.net/problem=41
 *
 * What is the largest n-digit pandigital prime that exists?
 */
object Problem_0041 extends Problem {

  val pandigitalPrimes = for {
    n <- 9 to 1 by -1
    permStr <- (1 to n).mkString.permutations
    perm = permStr.toInt
    if isPrime(perm)
  } yield perm

  def answer = pandigitalPrimes.max
}
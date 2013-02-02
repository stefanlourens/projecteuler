package problems

import Problem_0003.isPrime

/**
 * http://projecteuler.net/problem=49
 *
 */
object Problem_0049 extends Problem {

  def primesFrom(n: Int) = for {
    n <- (n to Int.MaxValue by 2).toStream
    if (isPrime(n))
  } yield n

  val fourDigitPrimes = primesFrom(1485).takeWhile { _ < 10000 }
  val possibles = for {
    n <- fourDigitPrimes
    val permutations = n.toString.permutations.map { _.toInt }.toList
    val possibles = fourDigitPrimes.intersect(permutations).toList.sorted.reverse
    if (possibles.length >= 3)
  } yield possibles
  
  val head =  possibles.head
  
  //@TODO : Find numbers with commons difference

  def answer = ???
  
}
package problems

import Problem_0003.isPrime

/**
 * http://projecteuler.net/problem=49
 * 
 * The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways: 
 * (i) each of the three terms are prime, and, 
 * (ii) each of the 4-digit numbers are permutations of one another.
 *
 * There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property, but there is one other 4-digit increasing sequence.
 *
 * What 12-digit number do you form by concatenating the three terms in this sequence?
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
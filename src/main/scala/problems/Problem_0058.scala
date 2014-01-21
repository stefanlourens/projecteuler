package problems

import problems.Problem_0003.isPrime
import problems.Problem_0028.diagonals

/**
 * http://projecteuler.net/problem=58
 *
 * What is the side length of the square spiral for which the ratio of primes along both diagonals first falls below 10%?
 */
object Problem_0058 extends Problem {

  case class Diagonal(n: Long, count: Long, primeCount: Long)

  val diagonalEntries: Stream[Diagonal] = {
    var primeCount: Long = 0
    var count: Long = 1
    diagonals.map { n =>
      count = count + 1
      if (isPrime(n)) primeCount = primeCount + 1

      Diagonal(n, count, primeCount)
    }
  }

  def answer = {
    val diagonalCount = diagonalEntries.drop(13).dropWhile{
      d => (d.primeCount.toFloat / d.count) >= 0.10
    }.head.count

    (diagonalCount + 1) / 2
  }

}
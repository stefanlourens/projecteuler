package problems

import Problem_0010.primes

/**
 * http://projecteuler.net/problem=50
 *
 * Which prime, below one-million, can be written as the sum of the most consecutive primes?
 */
object Problem_0050 extends Problem {

  val primesBelow = 41
  val primeRange = primes takeWhile(_ < primesBelow)

  def calcPrimeSumsFor(prime: Long): Int = {
    def calcPrimeSumsFor(prime: Long, primesLeft: List[Long], count: Int, max: Int): Int = {
      val sum = primesLeft.take(count).sum
        if (sum == prime) calcPrimeSumsFor(prime, primesLeft.tail, 0, if (count > max) count else max)
      else if (sum > prime || count > primesLeft.size || max > primesLeft.size) calcPrimeSumsFor(prime, primesLeft.tail, 0, max)
      else if (primesLeft.isEmpty || max >= primesLeft.size) max
      else calcPrimeSumsFor(prime, primesLeft, count + 1, max)
    }

    calcPrimeSumsFor(prime, primeRange.takeWhile(_ < prime).toList, 0, 0)
  }


  def answer = {
    0
  }
//    2 + 3 + 5 + 7 + 11 + 13 = 41
//
//    val primeSums: (Long, Int) = primeRange.foldLeft((0l, 0)){ case ((sum, count), prime) => (prime + sum, count + 1) }
//
//    primeSums

}
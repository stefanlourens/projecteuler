package problems

import scala.collection.mutable

/**
 * http://projecteuler.net/problem=47
 *
 * Find the first four consecutive integers to have four distinct prime factors. What is the first of these numbers?
 */
object Problem_0047 extends Problem {

  val primes = Problem_0010.primes

  def isPrime(n: Long): Boolean = primes.takeWhile(_ <= n).contains(n)

  def primeFactors(n: Long): Set[Long] = {
    def primeFactors(n: Long, currPrimes: Stream[Long], acc: Set[Long]): Set[Long] = {
      if (n == 0 || n == 1) acc
      else if (n % currPrimes.head == 0) primeFactors(n / currPrimes.head, primes,  acc + currPrimes.head)
      else primeFactors(n, currPrimes.tail, acc)
    }

    primeFactors(n, primes.takeWhile(_ <= n), Set())
  }

  case class FactorDetail(n: Long) {
    lazy val factors: Set[Long] = primeFactors(n)
    lazy val factorCount: Int = factors.size
  }

  val factorDetails = Stream.from(1) map (FactorDetail(_))

  //Num consecutive integers
  val numConsecutive, numFactors = 4

  def find(nums: Stream[FactorDetail]): Long = {
//    println(nums.head)
    if (nums.isEmpty) throw new Error("None found")
    else {
      val slice = nums take numConsecutive

      if (slice.forall(_.factorCount == numFactors)) slice.head.n
      else find(nums.tail)
    }
  }

    def answer = find(factorDetails)


}
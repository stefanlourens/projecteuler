package problems

import Problem_0003.isPrime

/**
 * http://projecteuler.net/problem=35
 *
 * How many circular primes are there below one million?
 */
object Problem_0035 extends Problem {

  val primes = for {
    n <- 2 #:: (3 to Int.MaxValue by 2).toStream
    if (isPrime(n))
  } yield n

  def rotate(n: Int): Set[Int] = {
    def rotate(n: String, rotationsLeft: Int, acc: Set[Int]): Set[Int] =
      if (rotationsLeft == 0) acc
      else {
        val rot = n.tail :+ n.head
        rotate(rot, rotationsLeft - 1, acc + rot.toInt)
      }

    val strRep = n.toString
    rotate(strRep, strRep.length, Set())
  }

  val candidates = primes.takeWhile { _ < 1000000 }.toSet

  def answer = candidates.par.filter {
    n => rotate(n).forall { candidates.contains(_) }
  }.size
}
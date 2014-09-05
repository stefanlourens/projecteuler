package problems

import Problem_0002.fibs

/**
 * http://projecteuler.net/problem=104
 *
 * Given that Fk is the first Fibonacci number for which the first nine digits AND the last nine digits are 1-9 pandigital, find k.
 */
object Problem_0104 extends Problem {

  val fibsWithIndex = fibs.map(_.toString).zipWithIndex

  def isPandigital(n: String): Boolean = n.length == 9 && (1 to 9 forall(pdn => n.contains(pdn.toString)))

  def answer = fibsWithIndex.find { case (n, idx) =>
    n.length > 9 * 2 && isPandigital(n.take(9)) && isPandigital(n.takeRight(9))
  }

}
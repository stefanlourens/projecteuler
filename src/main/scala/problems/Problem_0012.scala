package problems

import math.{ sqrt, round }

/**
 * http://projecteuler.net/problem=12
 *
 * What is the value of the first triangle number to have over five hundred divisors?
 */
object Problem_0012 extends Problem {

  def divisorCount(n: Int): Int =
    (round(sqrt(n)) to 1 by -1 count (n % _ == 0)) * 2

  lazy val triangles: Stream[Int] = 1 #:: triangles.zipWithIndex.map { n => n._1 + n._2 + 2 }

  def answer = (triangles find {
    divisorCount(_) > 500
  }).get

}
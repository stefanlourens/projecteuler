package problems

import problems.Problem_0015.pascalTriangle

/**
 * http://projecteuler.net/problem=148
 *
 * Find the number of entries which are not divisible by 7 in the first one billion (10^9) rows of Pascal's triangle.
 */
object Problem_0148 extends Problem {

  def answer = {
    val it = pascalTriangle.iterator

    for {
      x <- 1 to 100000
      y <- 1 to 100000
    } yield it.next().foldLeft(0) { case (sum, n) => if (n % 7 != 0 ) sum + 1 else sum }
  }.foldLeft(0l) { _ + _ }

}
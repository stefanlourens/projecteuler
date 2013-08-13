package problems

import math.{abs, pow}
/**
 * Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
 */
object Problem_0006 extends Problem {

  def answer = {
    val range = 1 to 100
    val sumOfSquares = (range map (n => n * n)).sum
    val squareOfSum = pow(range sum, 2).toInt

    abs(sumOfSquares - squareOfSum)
  }

}
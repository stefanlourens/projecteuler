package problems

import math.pow

/**
 * http://projecteuler.net/problem=31
 *
 */
object Problem_0030 extends Problem {

  val powers = (0 to 9).map { n => n -> pow(n.toDouble, 5).toInt }.toMap


  def answer = ???

}
package problems

import Character.getNumericValue

/**
 * http://projecteuler.net/problem=16
 *
 * 215 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
 * What is the sum of the digits of the number 2^1000?
 */
object Problem_0016 extends Problem {

  def answer = BigInt(2).pow(1000).toString map(getNumericValue(_)) sum

}
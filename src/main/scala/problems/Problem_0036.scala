package problems

/**
 * http://projecteuler.net/problem=36
 *
 * Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.
 */
object Problem_0036 extends Problem {

  def answer = 1 until 1000000 filter {
    n: Int =>
      {
        val str = n.toString
        val bin = n.toBinaryString

        str == str.reverse && bin == bin.reverse
      }
  }.sum
}
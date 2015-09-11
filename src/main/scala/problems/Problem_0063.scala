package problems

/**
 * http://projecteuler.net/problem=63
 *
 * How many n-digit positive integers exist which are also an nth power?
 */
object Problem_0063 extends Problem {

  def answer = Math.pow(16807, 1d/5)
}
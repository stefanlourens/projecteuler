package problems

/**
 * http://projecteuler.net/problem=24
 *
 * What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
 */
object Problem_0024 extends Problem {

  def answer = (0 to 9).mkString.permutations.drop(999999).next().toLong

}
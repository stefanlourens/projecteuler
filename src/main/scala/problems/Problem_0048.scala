package problems

/**
 * http://projecteuler.net/problem=48
 *
 */
object Problem_0048 extends Problem {

  def answer = ((1 to 1000).foldLeft(BigInt(0)) {
    case (tot, n) => tot + BigInt(n).pow(n)
  }.toString takeRight 10).toLong
}
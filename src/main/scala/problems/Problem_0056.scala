package problems

/**
 * http://projecteuler.net/problem=56
 *
 * Considering natural numbers of the form, a^b, where a, b  100, what is the maximum digital sum?
 */
object Problem_0056 extends Problem {

  implicit class Summable(val n: BigInt) extends AnyVal {
    def digitalSum: Int = { n.toString map { _.asDigit } }.sum
  }

  def answer = {
    val sums = for {
      a <- 1 to 100
      b <- 1 to 100
    } yield BigInt(a).pow(b).digitalSum

    sums.max
  }
}
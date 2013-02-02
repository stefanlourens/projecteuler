package problems

/**
 * http://projecteuler.net/problem=20
 *
 * Find the sum of the digits in the number 100!
 */
object Problem_0020 extends Problem {

  implicit class Factorable(val n: Int) extends AnyVal {
    def ! = (BigInt(1) to BigInt(n)).foldLeft(BigInt(1)) { _ * _ }
  }
  
  def answer = (100!).toString map { _.asDigit } sum
}
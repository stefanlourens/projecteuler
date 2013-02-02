package problems

/**
 * http://projecteuler.net/problem=55
 *
 * How many Lychrel numbers are there below ten-thousand?
 */
object Problem_0055 extends Problem {

  implicit class Reversable(val n: BigInt) extends AnyVal {
    def reverse: BigInt = BigInt(n.toString.reverse)
    def isPalindrome: Boolean = n == n.reverse

    def isLychrel: Boolean = {
      def isLychrel(n: BigInt, itCount: Int): Boolean = {
        val num = n + n.reverse

        if (itCount == 50) !num.isPalindrome
        else if (num.isPalindrome) false
        else isLychrel(num, itCount + 1)
      }

      isLychrel(n, 1)
    }
  }

  def answer = 1 until 10000 count { BigInt(_).isLychrel } 
}
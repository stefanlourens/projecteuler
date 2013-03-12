package problems

/**
 * A palindromic number reads the same both ways.
 * The largest palindrome made from the product of two 2-digit numbers is
 * 9009 = 91 × 99. Find the largest palindrome made from the product of two 3-digit numbers.
 */
object Problem_0004 extends Problem {

  def answer = {
    val threeDigits = 100 to 999
    val palindromes = for {
      x <- threeDigits
      y <- threeDigits
      val product = x * y
      if (product.toString == product.toString.reverse)
    } yield product

    palindromes.max
  }

}
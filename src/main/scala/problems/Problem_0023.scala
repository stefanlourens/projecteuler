package problems

/**
 * http://projecteuler.net/problem=23
 *
 * Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.
 */
object Problem_0023 extends Problem {

  def getDivisorSum(n: Int): Int = {
    1 until (n.toInt - 1) filter { n % _ == 0 } sum
  }

  def answer = {
    val nums = {
      for {
        n <- 1 to 10000
      } yield n -> getDivisorSum(n)
    }.toMap.withDefaultValue(0)

    (nums filter { case (n, divSum) => n != divSum && nums(divSum) == n }).keys.sum
  }
}
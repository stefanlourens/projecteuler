package problems

/**
 * http://projecteuler.net/problem=21
 *
 * Evaluate the sum of all the amicable numbers under 10000.
 */
object Problem_0021 extends Problem {

  def getDivisorSum(n: Int): Int = {
    1 until (n - 1) filter { n % _ == 0 } sum
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
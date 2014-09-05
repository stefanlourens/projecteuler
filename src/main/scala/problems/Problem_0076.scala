package problems

/**
 * http://projecteuler.net/problem=76
 *
 * How many different ways can one hundred be written as a sum of at least two positive integers?
 */
object Problem_0076 extends Problem {

  val coins = 1 until 100 toList

  def countChange(money: Int, coins: List[Int]): Int = {
    def cc(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (money < 0 || coins.isEmpty) 0
      else cc(money, coins.tail) + cc(money - coins.head, coins)
    }

    cc(money, coins.sorted)
  }

  def answer = countChange(100, coins)

}
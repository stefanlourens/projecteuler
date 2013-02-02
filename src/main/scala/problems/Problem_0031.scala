package problems

import math.pow

/**
 * http://projecteuler.net/problem=31
 *
 */
object Problem_0031 extends Problem {

  val coins = List(1, 2, 5, 10, 20, 50, 100, 200)

  def countChange(money: Int, coins: List[Int]): Int = {
    def cc(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (money < 0 || coins.isEmpty) 0
      else cc(money, coins.tail) + cc(money - coins.head, coins)
    }

    cc(money, coins.sorted)
  }

  def answer = countChange(200, coins)

}
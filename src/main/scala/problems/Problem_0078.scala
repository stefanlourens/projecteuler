package problems

/**
 * http://projecteuler.net/problem=78
 *
 * Find the least value of n for which p(n) is divisible by one million.
 */
object Problem_0078 extends Problem {


  def countChange(money: Int, coins: Seq[Int]): Int = {
    def cc(money: Int, coins: Seq[Int]): Int = {
      if (money == 0) 1
      else if (money < 0 || coins.isEmpty) 0
      else cc(money, coins.tail) + cc(money - coins.head, coins)
    }

    cc(money, coins.sorted)
  }

  val fibs = Problem_0002.fibs

  def answer = (1 to 50).toStream foreach { n =>
    println(n + " = " + countChange(n, 1 to n) + " F:" + fibs(n))
  }

}
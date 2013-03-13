package problems

import io.Source

/**
 * http://projecteuler.net/problem=42
 *
 * How many are triangle words?
 */
object Problem_0042 extends Problem {

  val source = Source.fromFile("resources/0042/words.txt").mkString
  val words = source.split(",").map { _.replaceAll("\"", "") }
  val ordinals = ('A' to 'Z') zip (1 to 26) toMap

  val triangleNumbers: Stream[Int] = {
    def triangleNumbersFrom(n: Int): Stream[Int] =
      (0.5 * n * (n + 1) toInt) #:: triangleNumbersFrom(n + 1)

    triangleNumbersFrom(1)
  }

  def answer = words map { _.map(ordinals).sum } filter {
    n: Int => triangleNumbers takeWhile { _ <= n } contains n
  } length

}
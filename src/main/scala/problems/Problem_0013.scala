package problems

import io.Source

/**
 * Work out the first ten digits of the sum of the following one-hundred 50-digit numbers.
 */
object Problem_0013 extends Problem {

  val numbers = {
    val source = Source.fromFile("resources/0013/numbers.txt")

    for {
      line <- source.getLines()
    } yield BigInt(line)
  }

  def answer = BigInt((numbers sum).toString take 10)

}
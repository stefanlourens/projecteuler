package problems

/**
 * http://projecteuler.net/problem=52
 * 
 * It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits, but in a different order.
 *
 * Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.
 */
object Problem_0052 extends Problem {

  val numbers = 1 to Int.MaxValue toStream

  def answer = numbers.find {
    n =>
      {
        val str = n.toString
        (2 to 6) map { n * _ } forall { i => (i.toString diff str).isEmpty }
      }
  }.get

}
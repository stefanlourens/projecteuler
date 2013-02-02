package problems

/**
 * http://projecteuler.net/problem=52
 *
 */
object Problem_0052 extends Problem {

  val numbers = 1 to Int.MaxValue toStream

  def answer = numbers.find {
    n =>
      {
        val str = n.toString
        (2 to 6) map { n * _ } forall { i => (i.toString diff str).isEmpty }
      }
  }

}
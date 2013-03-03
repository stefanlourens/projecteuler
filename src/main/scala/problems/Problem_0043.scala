package problems

/**
 * http://projecteuler.net/problem=43
 *
 * Find the sum of all 0 to 9 pandigital numbers with this property.
 */
object Problem_0043 extends Problem {

  val pandigitals = (0 to 9).mkString.permutations filter { _.head != '0' }
  val indexes = 1 to 7 zip List(2, 3, 5, 7, 11, 13, 17)

  def answer = {
    val numbers = for {
      pan <- pandigitals
      if (indexes.forall { case (idx, div) => pan.substring(idx, idx + 3).toInt % div == 0 })
    } yield pan.toLong

    numbers sum
  }

}
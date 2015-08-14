package problems

/**
 * http://projecteuler.net/problem=32
 *
 * Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.
 */
object Problem_0038 extends Problem {

  def toInt(positional: Seq[Int]): Int = {
    require(positional.forall(_ < 10))
    (positional.reverse.zipWithIndex map { case (n, i) => n * math.pow(10, i).toInt}).sum
  }

  def answer = {
    val permutations = (1 to 9).permutations

    val products = for {
      permutation <- permutations
      x <- 1 to 3
      y <- 1 to 4
      if x < y
      product = toInt(permutation.take(x)) * toInt(permutation.drop(x).take(y))
      if product == toInt(permutation.drop(x + y))
    } yield product

    products.toSet.sum
  }

}
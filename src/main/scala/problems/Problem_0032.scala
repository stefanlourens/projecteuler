package problems

/**
 * http://projecteuler.net/problem=32
 *
 * Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.
 */
object Problem_0032 extends Problem {

  def toInt(positional: Seq[Int]): Int = {
    val based = positional.flatMap(Problem_0030.toDigits).reverse
      .zipWithIndex map { case (n, i) =>
      n * math.pow(10, i).toInt
    }
    based.sum
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
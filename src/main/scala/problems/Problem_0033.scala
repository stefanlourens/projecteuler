package problems

/**
 * http://projecteuler.net/problem=33
 *
 * There are exactly four non-trivial examples of this type of fraction, less than one in value, and containing two digits in the numerator and denominator.
 *
 * If the product of these four fractions is given in its lowest common terms, find the value of the denominator.
 */
object Problem_0033 extends Problem {

  def findFactors(n: Int): Seq[Int] = 2 to n filter (n % _ == 0)

  lazy val examples = for {
    x <- 1 to 9
    y <- 1 to 9
    z <- 1 to 9
    if x < z
    if (9 * x * z) + (y * z) == 10 * x * y
  } yield x -> z


  def answer = {
    val (nom, denom) = examples.foldLeft((1, 1)) { case ((tn, td), (n, d)) => tn * n -> td * d }

    denom / (findFactors(nom) intersect findFactors(denom)).max
  }

}
package problems

/**
 * There exists exactly one Pythagorean triplet for which a + b + c = 1000.
 * Find the product abc.
 *
 */
object Problem_0009 extends Problem {

  def answer =
    for {
      c <- 1 to 1000
      b <- 1 to c
      a <- 1 to b
      if (a + b + c == 1000)
      if ((a * a) + (b * b) == (c * c))
    } yield a * b * c

}
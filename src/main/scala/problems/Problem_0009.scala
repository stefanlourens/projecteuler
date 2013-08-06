package problems

/**
 * There exists exactly one Pythagorean triplet for which a + b + c = 1000.
 * Find the product abc.
 *
 */
object Problem_0009 extends Problem {

  def answer = {
    for {
      c <- (1 to 1000).toStream
      b <- (1 to c).toStream
      a <- (1 to b).toStream
      if a + b + c == 1000 && (a * a) + (b * b) == (c * c)
    } yield a * b * c
  }.head
}
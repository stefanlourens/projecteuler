package problems

import io.Source

/**
 * http://projecteuler.net/problem=59
 *
 * Find the sum of the ASCII values in the original text.
 */
object Problem_0059 extends Problem {

  val cipher = Source.fromFile("resources/0059/cipher1.txt").mkString.split(",") map { _.toInt } toList
  val allChars = ((' ' to '"') ++ ('\'' to ')') ++ (',' to '.') ++ ('0' to 'z')).map { _.toInt }.toSet
  val lowerChars = ('a' to 'z').map { _.toInt }

  def mapXOR(cipher: List[Int], key: List[Int]): List[Int] =
    (cipher zip Stream.continually(key).flatten).map { case (c, k) => c ^ k }

  def answer = {
    val keys = for {
      a <- lowerChars
      b <- lowerChars
      c <- lowerChars
      if (mapXOR(cipher, List(a, b, c)) forall { allChars.contains(_) })
    } yield List(a, b, c)

    mapXOR(cipher, keys(0)).sum
  }
}
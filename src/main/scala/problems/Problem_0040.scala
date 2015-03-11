package problems

/**
 * http://projecteuler.net/problem=40
 *
 * If dn represents the nth digit of the fractional part, find the value of the following expression.
 * d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000
 */
object Problem_0040 extends Problem {

  def answer = {

    val num = Stream.iterate(1)(_ + 1).take(1000000).mkString
    (Stream.iterate(1)(_ * 10) takeWhile ( _ <= 1000000) map (n => num.charAt(n - 1).asDigit)).product
  }
}
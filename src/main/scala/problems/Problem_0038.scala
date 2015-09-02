package problems

/**
 * http://projecteuler.net/problem=38
 *
 * What is the largest 1 to 9 pandigital 9-digit number that can be formed as the
 * concatenated product of an integer with (1,2, ... , n) where n > 1?
 */
object Problem_0038 extends Problem {

  def isPandigital(n: String): Boolean = {
    n.length == 9 && (1 to 9 forall (pdn => n.contains(pdn.toString)))
  }

  def toInt = Problem_0032.toInt _


//  Problem_0032.toInt()
//  Problem_0030.toDigits()



  def answer =
  (100 to 999 map(n => 1 to 3 map (_ * n)) filter(p => isPandigital(p.mkString)) map(toInt(_))).max
}
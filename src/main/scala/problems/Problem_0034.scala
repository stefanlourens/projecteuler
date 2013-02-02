package problems

/**
 * http://projecteuler.net/problem=34
 *
 */
object Problem_0034 extends Problem {

  implicit class Factorable(val n: Int) extends AnyVal {
    def ! = (BigInt(1) to BigInt(n)).foldLeft(BigInt(1)) { _ * _ }
  }
  
  val factorials = 0 to 9 map { n => (n, (1 to n).foldLeft(1){_ * _}) } toMap

  val numbers = 10 to Int.MaxValue
  numbers.length
  

  numbers map {
    n =>
      {
        (n, n.toString.map{ c => factorials(c.asDigit) }.sum)
      }
  } filter{ case (x, y) => x == y} foreach println

  def answer = 0
}
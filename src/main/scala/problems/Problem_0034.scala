package problems

/**
 * http://projecteuler.net/problem=34
 *
 */
object Problem_0034 extends Problem {

  implicit class Factorable(val n: Int) extends AnyVal {
    def ! = (BigInt(1) to BigInt(n)).foldLeft(BigInt(1)) { _ * _ }
  }

  val factorials = {
    0 to 9 map { n:Int => ((n + "").charAt(0), n!) }
  }.toMap

  val numbers = BigInt(10) to BigInt(Int.MaxValue) toStream


  for {
    number <- numbers
    if (number == number.toString.map(factorials).sum)
  } yield number

  numbers map { n =>
      (n, n.toString.map(factorials).sum)
  } filter{ case (x, y) => x == y} foreach println

  def answer = 0
}
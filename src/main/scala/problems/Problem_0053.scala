package problems

/**
 * http://projecteuler.net/problem=53
 *
 * Combinatoric selections
 */
object Problem_0053 extends Problem {

  implicit class Factorable(val n: Int) extends AnyVal {
    def ! = (BigInt(1) to BigInt(n)).foldLeft(BigInt(1)) { _ * _ }
  }

//  Could be done quicker using pascal's triangle
//  lazy val pascalTriangle: Stream[List[Long]] = List(1L) #:: List(1L, 2L, 1L) #:: pascalTriangle.tail.map {
//    row => (row ::: List(0L)) zip (0L :: row) map { case (x, y) => x + y }
//  }
  

  def answer = {
    for {
      n <- 1 to 100
      r <- 1 to (n - 1)
      val numValues = (n!) / ((r!) * ((n - r)!))
      if (numValues > 1000000)
    } yield numValues
  }.length

}
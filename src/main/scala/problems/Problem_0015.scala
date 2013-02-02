package problems

/**
 * http://projecteuler.net/problem=15
 *
 * How many routes are there through a 2020 grid?
 */
object Problem_0015 extends Problem {

  lazy val pascalTriangle: Stream[List[Long]] = List(1L) #:: List(1L, 2L, 1L) #:: pascalTriangle.tail.map {
    row => (row ::: List(0L)) zip (0L :: row) map { case (x, y) => x + y }
  }

  def answer = pascalTriangle(2 * 20 -1)(20)

}
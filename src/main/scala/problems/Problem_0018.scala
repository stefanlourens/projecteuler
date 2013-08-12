package problems

import io.Source


/**
 * http://projecteuler.net/problem=18
 *
 * Find the maximum total from top to bottom of the triangle below
 */
object Problem_0018 extends Problem {

  def findMaxPathSum(triangle: Array[Array[Int]]): Int = {
    for {
      row <- triangle.length - 2 to 0 by -1
      col <- 0 until triangle(row).length
    } {
      val curr = triangle(row)(col)
      val left = triangle(row + 1)(col)
      val right = triangle(row + 1)(col + 1)

      triangle(row).update(col, curr + (left max right))
    }

    triangle(0)(0)
  }

  val triangle: Array[Array[Int]] =
    Source.fromFile("resources/0018/triangle.txt").getLines().toArray map{ _.split(" ").map{ _.toInt } }

  def answer = findMaxPathSum(triangle)

}
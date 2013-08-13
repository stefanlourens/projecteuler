package problems

import io.Source
import problems.Problem_0018.findMaxPathSum


/**
 * http://projecteuler.net/problem=18
 *
 * Find the maximum total from top to bottom of the triangle below
 */
object Problem_0067 extends Problem {

  val triangle: Array[Array[Int]] =
    Source.fromFile("resources/0067/triangle.txt").getLines().toArray map{ _.split(" ").map{ _.toInt } }

  def answer = findMaxPathSum(triangle)

}
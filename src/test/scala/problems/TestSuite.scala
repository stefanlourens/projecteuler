package problems

import org.scalatest.FunSuite
import problems._

class ProblemsTestSuite extends FunSuite {

  def companion[T](name: String)(implicit man: Manifest[T]): T =
    Class.forName(name + "$").getField("MODULE$").get(man.erasure).asInstanceOf[T]

  val result = companion[Problem]("problems.Problem_0001").answer

  val answers = Map(
    1  -> 233168,
    2  -> 4613732,
    3  -> 6857,
    4  -> 906609,
    5  -> 232792560,
    6  -> 25164150,
    7  -> 104743,
    8  -> 40824,
    9  -> 31875000,
    10 -> 142913828922l,
    11 -> 70600674,
    12 -> 76576500,
    13 -> 5537376230l,
    14 -> 837799
    )

  for {
    n <- answers.keySet.toList.sorted
    val className = "problems.Problem_" + format("%04d", n)
  } test(className) {
    companion[Problem](className).answer === answers(n)
  }

}

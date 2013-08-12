package problems

import org.scalatest.FunSuite
import problems._

class ProblemsTestSuite extends FunSuite {

  def companion[T](name: String)(implicit man: Manifest[T]): T =
    Class.forName(name + "$").getField("MODULE$").get(man.runtimeClass).asInstanceOf[T]

  val answers = Map(
    1  -> 233168,
    2  -> 4613732,
    3  -> 6857,
    4  -> 906609,
    5  -> 232792560,
    6  -> 25164150,
    7  -> 104743,
    8  -> 40824,
    9  -> 31875000L,
    10 -> 142913828922L,
    11 -> 70600674,
    12 -> 76576500,
    13 -> 5537376230L,
    14 -> 837799,
    15 -> 137846528820L,
    16 -> 1366,
    17 -> 21124,
    18 -> 1074,
    19 -> 171,
    20 -> 648,
    21 -> 31626,
    22 -> 871198282L,
    24 -> 2783915460L,
    25 -> 4782,
    29 -> 9183,
    31 -> 73682,
    35 -> 55,
    36 -> 872187,
    37 -> 748317,
    41 -> 7652413,
    42 -> 162,
    43 -> 16695334890L,
    48 -> 9110846700L,
    52 -> 142857,
    53 -> 4075,
    54 -> 376,
    55 -> 249,
    56 -> 972,
    59 -> 107359,
    67 -> 7273

    )

  //Print problems not solved
  val missingProblems = for {
    n <- 1 to answers.keySet.max
    if !answers.contains(n)
  } yield n

  println(s"${Console.WHITE}Missing problems ${Console.BLUE}${missingProblems.mkString(",")}")

  for {
    n <- answers.keySet.toList.sorted
    className = "problems.Problem_" + "%04d".format(n)
  } test(className) {
      val tr = time(companion[Problem](className).answer)
      val duration = tr.duration.toLong

      val color = duration match {
        case x if x < 2000 => Console.GREEN
        case x if x < 5000 => Console.YELLOW
        case _ => Console.RED
      }

      println(s"${Console.WHITE}$className took $color${duration}ms")

      assert(tr.result === answers(n))
    }

}

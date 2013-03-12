package problems

/**
 * http://projecteuler.net/problem=17
 *
 * If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?
 */
object Problem_0017 extends Problem {

  val numerals = Map(
    1 -> "one",
    2 -> "two",
    3 -> "three",
    4 -> "four",
    5 -> "five",
    6 -> "six",
    7 -> "seven",
    8 -> "eight",
    9 -> "nine",
    10 -> "ten",
    11 -> "eleven",
    12 -> "twelve",
    13 -> "thirteen",
    14 -> "fourteen",
    15 -> "fifteen",
    16 -> "sixteen",
    17 -> "seventeen",
    18 -> "eighteen",
    19 -> "nineteen",
    20 -> "twenty",
    30 -> "thirty",
    40 -> "forty",
    50 -> "fifty",
    60 -> "sixty",
    70 -> "seventy",
    80 -> "eighty",
    90 -> "nighty",
    100 -> "hundred",
    1000 -> "thousand")

  def toNumeral(num: Int): List[String] = {
    def toNumeral(remain: Int, steps: List[Int], acc: List[String]): List[String] = {
      if (remain == 0) acc
      else if (remain < 100 && remain != 1000 && numerals.contains(remain)) acc :+ numerals(remain)
      else {
        val step = steps.head
        val divCount = remain / step
        val remainder = remain % step

        if (step == 10 && divCount > 1)
          toNumeral(remainder, steps.tail, acc :+ numerals(divCount * step))
        else if (divCount > 0)
          toNumeral(remainder, steps.tail, acc :+ numerals(divCount) :+ numerals(steps.head))
        else
          toNumeral(remainder, steps.tail, acc)
      }
    }

    toNumeral(num, List(1000, 100, 10, 1), List())
  }

  def answer = (1 to 1000).map {
    num =>
      {
        val numeral = toNumeral(num)
        if (numeral.length > 2) numeral.mkString.length + 3
        else numeral.mkString.length
      }
  }.sum

}
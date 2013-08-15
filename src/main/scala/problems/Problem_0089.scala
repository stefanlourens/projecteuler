package problems

import io.Source
import scala.collection.SortedMap
import scala.annotation.tailrec

/**
 * http://projecteuler.net/problem=89
 *
 * Roman Numerals : Find the number of characters saved by writing each of these in their minimal form.
 */
object Problem_0089 extends Problem {

  val numerals = Source.fromFile("resources/0089/roman.txt").getLines()

  val numeralValues = Map[Char, Int](
    'I' -> 1,
    'V' -> 5,
    'X' -> 10,
    'L' -> 50,
    'C' -> 100,
    'D' -> 500,
    'M' -> 1000
   )

  val subtractiveNumerals = SortedMap[Int, String](
    900 -> "CM",
    400 -> "CD",
    90 -> "XC",
    40 -> "XL",
    9 -> "IX",
    4 -> "IV"
  )(Ordering[Int].reverse)


  def numeralToInt(numeral: String): Int = {
    @tailrec
    def calcTotal(numeral: String, lastMax: Int, acc: Int): Int = {
      if (numeral.isEmpty) acc
      else {
        val curr = numeralValues(numeral.head)

        if (curr >= lastMax) {
          calcTotal(numeral.tail, curr, acc + curr)
        } else {
          calcTotal(numeral.tail, lastMax, acc - curr)
        }
      }
    }

    calcTotal(numeral.reverse, 0, 0)
  }


  def toNumeral(n: Int): String = {
      @tailrec
      def toNumeral(n: Int, pos: SortedMap[Int, Char], acc: String): String = {
        if (n == 0) acc
        else {
          val (intVal, numeral) = pos.head
          val divs = n / intVal
          val remain = n % intVal
          val newAcc = acc + (numeral.toString() * divs)

          lazy val nextPos = if (!pos.tail.isEmpty) pos.tail.head._1 else 0
          lazy val subPossibles = subtractiveNumerals.filter{ case (i, num) => i > nextPos && remain / i > 0 }
          lazy val sub = {
            if (!subPossibles.isEmpty)
              Option(subPossibles.map { case (i, num) => remain / i  -> (i, num)}.min)
            else None
          }

          if (subtractiveNumerals.contains(remain)) {
            newAcc + subtractiveNumerals(remain)
          } else if (sub.isDefined) {
            val (intV, numV) = sub.get._2
            toNumeral(remain - intV, pos.tail, newAcc + numV)
          } else {
            toNumeral(remain, pos.tail, newAcc)
          }
        }
      }

    toNumeral(n, SortedMap(numeralValues.map { _.swap }.toArray: _*)(Ordering[Int].reverse), "")

  }

  def answer: Any = numerals.map {
    numeral => {
      val currLength = numeral.size
      val newLength = toNumeral(numeralToInt(numeral)).size
      currLength - newLength
    }
   }.sum

}
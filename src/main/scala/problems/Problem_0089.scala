package problems

import io.Source
import collection.immutable.TreeMap

/**
 * http://projecteuler.net/problem=89
 *
 * Roman Numerals : Find the number of characters saved by writing each of these in their minimal form.
 */
object Problem_0089 extends Problem {

  val numerals = Source.fromFile("/Users/stefan/Projects/Personal/projecteuler/resources/0089/roman.txt") getLines
  val numeralValues = Map[Char, Int](
    'I' -> 1,
    'V' -> 5,
    'X' -> 10,
    'L' -> 50,  
    'C' -> 100,
    'D' -> 500,
    'M' -> 1000)
    
  val valueNumerals = TreeMap(numeralValues.map { _.swap }.toArray: _*)
  val intPositions = valueNumerals.keys.toList.reverse

  def numeralToInt(numeral: String): Int = {
    val values = numeral map numeralValues toList

    def calcTotal(values: List[Int], acc: List[Int]): Int = {
      if (values.isEmpty) acc sum
      else {
        val head = values.head
        val tail = values.tail
        if (!tail.isEmpty && head < tail.head)
          if (tail.tail.isEmpty) calcTotal(List(), (tail.head - head) :: acc)
          else calcTotal(tail.tail, tail.head - head :: acc)
        else calcTotal(tail, head :: acc)
      }
    }

    calcTotal(values, List())  
  }

  /*
  def toNumeral(n: Int): String = {
    def toNumeral(n: Int, positions: List[Int], acc: List[Char]): String = {
      if (n <= 0) acc mkString
      else {
        val divisor = positions.head
        val divisions = n / divisor

        if (divisions == 0) toNumeral(n, positions.tail, acc)
        else {
          val updatedAcc = acc ++ List.fill(divisions)(valueNumerals(divisor));

          if (divisor != intPositions.max) {
            val nextPos = intPositions.reverse.dropWhile { _ <= divisor }.headOption

            if (nextPos.isEmpty) toNumeral(n % divisor, positions.tail, updatedAcc)
            else {
              val diff = nextPos.get - (divisions * divisor)
              val prefix = valueNumerals find { _._1 / diff > 0 }
              
              if (prefix.isDefined && prefix.get._1 / diff < 3) {
              	val divs = prefix.get._1 / diff
              	toNumeral(n % divisor, positions.tail, acc ++ List.fill(divs)(prefix.get._2) :+ valueNumerals(nextPos.get))
              } else toNumeral(n % divisor, positions.tail, updatedAcc)

            }
          } else toNumeral(n % divisor, positions.tail, updatedAcc)
        }
      }

    }

    toNumeral(n, intPositions, List())
  }                 
  */ 

}
package problems

import io.Source

/**
 * http://projecteuler.net/problem=22
 *
 * What is the total of all the name scores in the file?
 */
object Problem_0022 extends Problem {

  val source = Source.fromFile("resources/0022/names.txt") mkString
  val names = source.split(",").map { _.replaceAll("\"", "") }.sorted.zipWithIndex
  
  val ordinals = ('A' to 'Z') zip (1 to 26) toMap

  def answer = names map { 
    case (name, idx) => 
      name.map(ordinals).sum * (idx + 1) 
   } sum
  
}
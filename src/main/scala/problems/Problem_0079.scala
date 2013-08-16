package problems

import io.Source
import scala.collection.mutable

/**
 * http://projecteuler.net/problem=79
 *
 * Given that the three characters are always asked for in order,
 * analyse the file so as to determine the shortest possible secret passcode of unknown length
 */
object Problem_0079 extends Problem {

  val logs = Source.fromFile("resources/0079/keylog.txt").getLines().toList
  val valsBefore = new mutable.HashMap[Char, Set[Char]]()

  def answer = {
    for {
      row <- logs
      n <- 0 until row.size
    } {
      val c = row(n)
      valsBefore.put(c, valsBefore.getOrElse(c, Set()) ++ row.dropRight(row.size - n).toSet)
    }

    valsBefore.toList.sortBy(_._2.size).map(_._1).mkString.toInt
  }

}
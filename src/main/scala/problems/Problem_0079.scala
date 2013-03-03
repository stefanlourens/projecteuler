package problems

/**
 * http://projecteuler.net/problem=79
 *
 * Given that the three characters are always asked for in order,
 * analyse the file so as to determine the shortest possible secret passcode of unknown length
 */
object Problem_0079 extends Problem {

  val logs = Source.fromFile("/Users/stefan/Projects/Personal/projecteuler/resources/0079/keylog.txt").getLines.toList

  val logs = Source.fromFile("/Users/stefan/Projects/Personal/projecteuler/resources/0079/keylog.txt").getLines.toList
  
  val charWithIndexSum = logs.flatMap{ _.zipWithIndex } groupBy{ _._1 } map {
  	case (k, v) => {
  		(k, v.foldLeft(0){ (s, c) => s + c._2 } - v.length, v.foldLeft(0){ (s, c) => s + c._2 })
  	}
  } toList
  
  charWithIndexSum.sortBy( _._2 ) map(_._1) mkString
  
  def answer = ??? //pathCountFrom(0, grid.origin)new Numeral(100)

}
package problems

/**
 * http://projecteuler.net/problem=25
 *
 * What is the first term in the Fibonacci sequence to contain 1000 digits?
 */
object Problem_0025 extends Problem {

  lazy val fibs: Stream[BigInt] = BigInt(1) #:: BigInt(1) #:: fibs.zip(fibs.tail).map { n => n._1 + n._2 }

  def answer = fibs.indexWhere { _.toString().length == 1000 } + 1

}
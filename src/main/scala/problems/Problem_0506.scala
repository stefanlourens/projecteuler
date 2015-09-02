package problems

/**
 * https://projecteuler.net/problem=506
 *Consider the infinite repeating sequence of digits:
 * 1234321234321234321...
 *
 * Amazingly, you can break this sequence of digits into a sequence of integers such that the sum of the digits in the n'th value is n.
 *
 * The sequence goes as follows:
 * 1, 2, 3, 4, 32, 123, 43, 2123, 432, 1234, 32123, ...
 *
 * Let vn be the n'th value in this sequence. For example, v2 = 2, v5 = 32 and v11 = 32123.
 *
 * Let S(n) be v1 + v2 + ... + vn. For example, S(11) = 36120, and S(1000) mod 123454321 = 18232686.
 *
 * Find S(1014) mod 123454321.
 *
 * Find S(1014) mod 123454321.
 */
object Problem_0506 extends Problem {

  def toInt(positional: Seq[BigInt]): BigInt = {
    require(positional.forall(_ < 10))
    (positional.reverse.zipWithIndex map { case (n, i) => n * BigInt(10).pow(i)}).sum
  }


  def series: Stream[BigInt] = {
    val digits = Stream.continually(List(1, 2, 3, 4, 3, 2).map(BigInt(_)).toStream).flatten

    def series(pos: Int, digits: Stream[BigInt]): Stream[BigInt] = {
      var sum = BigInt(0)

      val taken = digits.takeWhile { n =>
        sum = sum + n
        sum <= pos
      }

      toInt(taken) #:: series(pos + 1, digits.drop(taken.length))
    }

    series(1, digits)
  }

  println(series.take(1000).sum % 123454321)

  val list = series.take(100).toList
  var sum = BigInt(0)

  println("pos,num,sum,len,mod" )
  list.zipWithIndex.foreach{ case (n, i) =>
      sum  = sum + n
//      println(s"\tpos:$i: \t\t sum:$sum \t\t len:${n.toString.length} \t\t mod:${n % 123454321} \t\t  n:$n" )
    println(s"${i + 1},$n,$sum,${n.toString.length},${n % 123454321}" )
  }


  def answer = 0

}
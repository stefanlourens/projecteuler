package problems

import annotation.tailrec

/**
 * http://projecteuler.net/problem=45
 *
 * Find the next triangle number that is also pentagonal and hexagonal.
 */
object Problem_0045 extends Problem {

  def seqFrom(n: Int, f: (Int) => Int): Stream[Int] = {
    f(n) #:: seqFrom(n + 1, f)
  }

  val tri = seqFrom(286, (n: Int) => (0.5 * n * (n + 1)).toInt)
  val pen = seqFrom(166, (n: Int) => n * (3 * n - 1) / 2)
  val hex = seqFrom(144, (n: Int) => n * (2 * n - 1))

  @tailrec
  def findCommon(seqs: List[Stream[Int]]): Int = {
    val max = seqs.maxBy { _.head }.head

    if (seqs forall { _.head == max }) max
    else {
      findCommon(seqs map { seq => seq.dropWhile { _ < max } })
    }
  }

  def answer = ???//findCommon(List(tri, pen, hex))
}
package problems

import annotation.tailrec

/**
 * http://projecteuler.net/problem=14
 *
 * Which starting number, under one million, produces the longest chain?
 */
object Problem_0014 extends Problem {

  
  def seqLength(n: Long): Int = {
    
    @tailrec
    def seqLength(n: Long, currLength: Int): Int = {
      if (n == 1) currLength + 1
      else if (n % 2 == 0) seqLength(n / 2, currLength + 1)
      else seqLength((3 * n) + 1, currLength + 1)
    }
    
    seqLength(n, 0)
  } 

  def answer = {
    val seqs = (500001 until 1000000).par map(n => (n, seqLength(n)))
    seqs maxBy{ case (_, length) => length } _1
  }

}
package problems

import scala.annotation.tailrec

/**
 * 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
 *
 * What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
 */
object Problem_0005 extends Problem {

  lazy val divisibleRange = 1 to 20
  
  def answer = {
    
    @tailrec
    def findDivisor(candidate: Int): Int = {
      if (divisibleRange forall (candidate % _ == 0)) candidate
      else findDivisor(candidate + 20)
    }

    findDivisor(20)
  }

}
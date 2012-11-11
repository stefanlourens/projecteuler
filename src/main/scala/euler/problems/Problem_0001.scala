package euler.problems

/*
 * If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
 *
 * Find the sum of all the multiples of 3 or 5 below 1000.
 */
object Problem_0001 extends Problem {
  
  def solveFor(limit: Int) = { 
    for {
      x <- 1 until limit
      if x % 3 == 0 || x % 5 == 0
    } yield x
  } sum
  
  def answer = solveFor(1000)

}
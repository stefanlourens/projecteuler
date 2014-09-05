package problems

/**
 * http://projecteuler.net/problem=28
 *
 * What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?
 */
object Problem_0028 extends Problem {

  //Compute the diagonals of the grid
  val diagonals: Stream[Long] = {
    var lastNum = 1
    var addCount = 0
    var add = 2

    def nextNum(): Stream[Long] = {
      if (addCount == 4) {
        addCount = 0
        add = add + 2
      }

      addCount = addCount + 1
      lastNum = lastNum + add
      lastNum #:: nextNum()
    }

    1l #:: nextNum()
  }

  def answer = (diagonals take ((1001 * 2) - 1)).sum

}
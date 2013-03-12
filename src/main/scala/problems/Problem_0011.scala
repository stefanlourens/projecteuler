package problems

import io.Source

/**
 * What is the greatest product of four adjacent numbers in
 * any direction (up, down, left, right, or diagonally) in the 2020 grid?
 */
object Problem_0011 extends Problem {

  type Matrix = Array[Array[Int]]

  val matrix: Matrix = {
    val source = Source.fromFile("resources/0011/grid.txt")

    {
      for {
        line <- source.getLines()
      } yield line split (" ") map (Integer.parseInt(_))
    }.toArray
  }

  lazy val horizontal = for {
    row <- matrix
    (_, idx) <- row.zipWithIndex
    if (idx < 16)
  } yield (row drop (idx) take (4)).product

  lazy val vertical = for {
    (row, rowIdx) <- matrix.zipWithIndex
    (col, colIdx) <- row.zipWithIndex
    if (rowIdx < 16)
  } yield (for (offset <- 0 until 4) yield matrix(rowIdx + offset)(colIdx)).product

  lazy val diagonalLR = for {
    (row, rowIdx) <- matrix.zipWithIndex
    (col, colIdx) <- row.zipWithIndex
    if (rowIdx < 16 && colIdx < 16)
  } yield (for (offset <- 0 until 4) yield matrix(rowIdx + offset)(colIdx + offset)).product

  lazy val diagonalRL = for {
    (row, rowIdx) <- matrix.zipWithIndex
    (col, colIdx) <- row.zipWithIndex
    if (rowIdx < 16 && colIdx > 3)
  } yield (for (offset <- 0 until 4) yield matrix(rowIdx + offset)(colIdx - offset)).product

  def answer = (horizontal ++ vertical ++ diagonalLR ++ diagonalRL).max

}
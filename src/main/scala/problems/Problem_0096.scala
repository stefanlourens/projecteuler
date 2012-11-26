package problems

import io.Source
import math.floor

/**
 * http://projecteuler.net/index.php?section=problems&id=96
 */
object Problem_0096 extends Problem {

  case class Cell(val value: Int, val row: Int, val col: Int) {
    def isEmpty: Boolean = value == 0
    def boxNum: Int = {
      val boxCol = floor(col / 3).toInt
      val boxRow = floor(row / 3).toInt
      (boxRow * 3) + boxCol + 1
    }
  }

  class Puzzle(val name: String, var cells: List[Cell]) {
    def row(y: Int): List[Cell] = cells filter { _.row == y }
    def col(x: Int): List[Cell] = cells filter { _.col == x }
    def box(n: Int): List[Cell] = cells filter { _.boxNum == n }
    def emptyCells: List[Cell] = cells filter { _.isEmpty }
    def isSolved: Boolean = (cells indexWhere { _.isEmpty }) == -1

    def patch(currCell: Cell, values: List[Int]): List[Puzzle] = {
      if (values.isEmpty) List()
      else {
        for {
          value <- values
        } yield {
          val (upto, after) = cells.splitAt((currCell.row * 9) + currCell.col)
          val puzzle = new Puzzle(name, upto ++ (new Cell(value, currCell.row, currCell.col) :: after.tail))
          println(puzzle.toConsoleString)
          puzzle
        }
      }
    }

    def possibleValues(cell: Cell): List[Int] = {
      def possibleValues(cell: Cell): List[Int] = {
        if (!cell.isEmpty) List(cell.value)
        else {
          val rowCells = row(cell.row)
          val colCells = col(cell.col)
          val boxCells = box(cell.boxNum)

          1 to 9 filterNot {
            possible =>
              (
                rowCells.indexWhere { _.value == possible } != -1
                || colCells.indexWhere { _.value == possible } != -1
                || boxCells.indexWhere { _.value == possible } != -1)
          } toList
        }
      }

      def neighborExludes(cell: Cell): List[Int] = {
        val neighbors = box(cell.boxNum) filter { _ != cell }
        val neighborValues = neighbors filterNot { _.isEmpty } map { _.value }

        val colValues = {
          for {
            colNum <- neighbors groupBy { _.col } keySet
          } yield col(colNum) filter { _.value != 0 } map { _.value }
        } flatten

        val rowValues = {
          for {
            row <- neighbors groupBy { _.row } keySet
          } yield col(row) filter { _.value != 0 } map { _.value }
        } flatten

        1 to 9 filterNot { colValues ++ rowValues ++ neighborValues } toList
      }

      possibleValues(cell) filter { neighborExludes(cell).contains(_) }
    }

    def solve: Puzzle = {

      def solve(emptyCells: List[Cell]): Puzzle = {
        if (emptyCells.isEmpty) this
        else {
          val currCell = emptyCells.head
          val posibleVals = possibleValues(currCell)

          val paths = {
            if (posibleVals.isEmpty) List()
            else {
              for {
                path <- patch(currCell, posibleVals)
              } yield {
                if (path.isSolved) { println("SOLVED\n" + path.toConsoleString); path }
                else path.solve
              }
            }
          }

          if (paths.isEmpty) null
          else paths.head
        }
      }

      solve(emptyCells)

      //      def findSolutions: List[Puzzle] =
      //        for {
      //          emptyCell <- emptyCells
      //          puzzle <- patch(emptyCell, possibleValues(emptyCell))
      //        } yield {
      //          if (puzzle.isSolved) {println(puzzle.toConsoleString);puzzle }
      //          else puzzle.solve
      //        }
      //        
      //        val solutions = findSolutions
      //        
      //        if (solutions.isEmpty) new Puzzle("empty", List())
      //        else solutions.head
    }

    //    def solve: Puzzle = {
    //      def solveCells(cellsToSolve: List[Cell]): Puzzle = {
    //        if (cellsToSolve.isEmpty) {
    //          if (!isSolved) { println("New Pass \n" + this); solveCells(cells) }
    //          else {
    //            println("Puzzle " + name + " solved")
    //            println(new Puzzle(name, cells.toList))
    //            this
    //          }
    //        } else {
    //          val cell = cellsToSolve.head
    //
    //          if (cell.isEmpty) {
    //            val values = possibleValues(cell)
    //            println(cell + " possibles : " + values)
    //
    //            if (values.size == 1) {
    //              println("Found value for cell " + values(0) + " cell = " + cell)
    //              val (upto, after) = cells.splitAt((cell.row * 9) + cell.col)
    //              cells = upto ++ (new Cell(values(0), cell.row, cell.col) :: after.tail)
    //              println(this)
    //            }
    //          }
    //
    //          solveCells(cellsToSolve.tail)
    //        }
    //      }
    //
    //      solveCells(cells)
    //    }

    override def toString = cells map {
      cell =>
        if (cell.col == 8) cell.value + "\n"
        else cell.value + " "
    } mkString

    def toConsoleString = cells map {
      cell =>
        {
          if (cell.isEmpty) Console.RED
          else Console.GREEN
        } +
          {
            if (cell.col == 8) cell.value + "\n"
            else cell.value + " "
          } +
          Console.RESET
    } mkString

  }

  private def parse(source: Source) = {
    val (names, grids) = source.getLines.partition(_ startsWith "Grid")
    for {
      (name, data) <- (names zip ((grids grouped 9))) toList
    } yield {
      val rows = (data map { _.zipWithIndex }).zipWithIndex

      val cells = for {
        (rowData, rowIdx) <- rows
        (c, colIdx) <- rowData
      } yield new Cell(Character.getNumericValue(c), rowIdx, colIdx)

      new Puzzle(name, cells.toList)
    }
  }

  //  private val source = Source.fromFile(new File("/Users/stefan/Projects/Personal/projecteuler/resources/96/sudoku_sample.txt"))
  private val source = Source.fromFile("/Users/stefan/Projects/Personal/projecteuler/resources/0096/sudoku.txt")
  val puzzles = parse(source)

  def answer = puzzles foreach { puzzle => println("Starting Puzzle " + puzzle.name); puzzle.solve }

}
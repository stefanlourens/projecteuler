package problems

import scala.io.Source

/**
 * http://projecteuler.net/index.php?section=problems&id=96
 */
object Problem_0096_bak extends Problem {

  case class Block(value: Int, row: Int, column: Int, subGrid: Int, index: Int) {
    val isEmpty: Boolean = value == 0
    val isDefined: Boolean = !isEmpty

    def updated(value: Int): Block = new Block(value, row, column, subGrid, index)

    class Test {
      def isValue: Boolean = value == 1
    }

    def test: Test = return new Test
  }

  sealed trait GridArea {
    val blocks: Vector[Block]
    lazy val filledBlocks: Vector[Block] = blocks filterNot (_.isEmpty)
    lazy val isValid: Boolean = !(filledBlocks groupBy (_.value) exists (_._2.size > 1))

    def contains(value: Int): Boolean = blocks exists (_.value == value)

    def emptyBlocks: Vector[Block] = blocks filter (_.isEmpty)

    def emptyBlocksExcluding(b: Block): Vector[Block] = emptyBlocks filterNot (_.index == b.index)

    def emptyBlocksExcluding(blocks: Vector[Block]): Vector[Block] = emptyBlocks filterNot (eb => blocks exists(_.index == eb.index))

    def emptyBlocksExcluding(p: Block => Boolean): Vector[Block] = emptyBlocks filterNot p

  }

  case class SubGrid(n: Int, blocks: Vector[Block]) extends GridArea

  sealed trait Lane extends GridArea {
    def intersectsSubGrid(subGrid: Int): Boolean = blocks exists (_.subGrid == subGrid)
  }

  case class Row(num: Int, blocks: Vector[Block]) extends Lane

  case class Column(num: Int, blocks: Vector[Block]) extends Lane

  object Grid {
    def apply(data: Seq[String]): Grid = {
      val gridData = (data zipWithIndex).tail map { case (rowData, rowIdx) =>
        (rowData zipWithIndex) map { case (c, colIdx) =>

          new Block(c.asDigit, rowIdx - 1, colIdx, (((rowIdx - 1) / 3) * 3) + (colIdx / 3), colIdx + ((rowIdx - 1) * 9))
        }
      }
      new Grid(gridData.flatten.toVector)
    }
  }

  case class Grid(blocks: Vector[Block]) extends GridArea {
    def updated(b: Block, value: Int): Grid = new Grid(blocks.updated(b.index, b.updated(value)))

    def findBlocks(p: Block => Boolean): Vector[Block] = blocks filter p

    def findBlock(index: Int): Block = (blocks find (_.index == index)).get

    def findBlock(row: Int, column: Int): Block = (blocks find (b => b.row == row && b.column == column)).get

    lazy val rows: IndexedSeq[Row] =
      0 until 9 map (row => new Row(row, findBlocks(_.row == row)))

    lazy val columns: IndexedSeq[Column] =
      0 until 9 map (col => new Column(col, findBlocks(_.column == col)))

    lazy val subGrids: IndexedSeq[SubGrid] =
      0 until 9 map (subGrid => new SubGrid(subGrid, findBlocks(_.subGrid == subGrid)))

    def siblingCols(b: Block): IndexedSeq[Column] =
      columns filter { column => column.num != b.column && column.intersectsSubGrid(b.subGrid)}

    def siblingRows(b: Block): IndexedSeq[Row] =
      rows filter { row => row.num != b.row && row.intersectsSubGrid(b.subGrid)}

    def containsAllNumbers(gridAreas: IndexedSeq[GridArea]): Boolean = gridAreas forall containsAllNumbers

    def containsAllNumbers(gridArea: GridArea): Boolean = {
      val values = (gridArea.blocks map (_.value)).toSet
      1 to 9 forall (values contains)
    }

    def eliminations(b: Block): Set[Int] = (rows(b.row).blocks ++ columns(b.column).blocks
      ++ subGrids(b.subGrid).blocks filterNot (_.isEmpty) map (_.value)).toSet

    def calcNonEliminatedValues(b: Block): List[Int] = {
      (1 to 9 filterNot (eliminations(b) contains)).toList
    }

    def findBlocksWithPossibleValue(v: Int, blocks: Vector[Block]): Vector[Block] = {
      blocks filter(calcNonEliminatedValues(_).contains(v))
    }


    /* TODO
    Naked Pairs : http://www.sudoku-solutions.com/solvingNakedSubsets.php#nakedPair
    Pointing Pairs : http://www.sudoku-solutions.com/solvingInteractions.php#pointingPair
    */
    def calcPossibleValues(b: Block): List[Int] = {
      val possibleValues = calcNonEliminatedValues(b)

      //Check if one if the possible values is the only one in the row or col or grid (Pinned Squares)
      val rowPossibles = (rows(b.row).emptyBlocksExcluding(b) flatMap calcNonEliminatedValues).toSet
      val colPossibles = (columns(b.column).emptyBlocksExcluding(b) flatMap calcNonEliminatedValues).toSet
      val gridPossibles = (subGrids(b.subGrid).emptyBlocksExcluding(b) flatMap calcNonEliminatedValues).toSet


      val pinnedValue =
        possibleValues find (pv => !rowPossibles.contains(pv) || !colPossibles.contains(pv) || !gridPossibles.contains(pv))

      //Check if any grid the row/col intersects only has suggestions for a number in that particular row/col (Intersection Removal)

      /**
       * Find empty blocks in the same row or col (but not in same grid) that have the same suggestion
       * check if those blocks (grouped by grid) are the only ones with that suggestion
       * if so remove from suggestions
       */
      val row = rows(b.row).emptyBlocksExcluding(_.subGrid == b.subGrid) groupBy(_.subGrid)
      val col = columns(b.column).emptyBlocksExcluding(_.subGrid == b.subGrid) groupBy(_.subGrid)

      val intersectionExcludesForRow = for {
        pv <- possibleValues
        (subGrid, blocks) <- row
        blocksWithSuggestion = findBlocksWithPossibleValue(pv, blocks)
        if blocksWithSuggestion.nonEmpty && findBlocksWithPossibleValue(pv, subGrids(subGrid).emptyBlocksExcluding(blocks)).isEmpty
      } yield pv

      val intersectionExcludesForCol = for {
        pv <- possibleValues
        (subGrid, blocks) <- col
        blocksWithSuggestion = findBlocksWithPossibleValue(pv, blocks)
        if blocksWithSuggestion.nonEmpty && findBlocksWithPossibleValue(pv, subGrids(subGrid).emptyBlocksExcluding(blocks)).isEmpty
      } yield pv

      val intersectionExcludes = intersectionExcludesForCol ++ intersectionExcludesForRow


      if (pinnedValue.isDefined) List(pinnedValue.get)
      else if (intersectionExcludes.nonEmpty) possibleValues filterNot(intersectionExcludes.contains(_))
      else possibleValues
    }

    lazy val possibleSubstitutions: Vector[(Block, List[Int])] = findBlocks(_.isEmpty) map (b => (b, calcPossibleValues(b)))

    lazy val isNotSolvable: Boolean = possibleSubstitutions exists { case (b, pv) => b.isEmpty && pv.isEmpty}

    lazy val isSolvable: Boolean = !isNotSolvable

    lazy val isSolved: Boolean = emptyBlocks.isEmpty && List(rows, columns, subGrids).forall(containsAllNumbers)

    override lazy val isValid: Boolean = rows ++ columns ++ subGrids forall (_.isValid)

    lazy val substitutions: Vector[(Block, Int)] = {
      println("\n\n\n\nSUBS\n" + this + "\n")
      for {
        (block, values) <- possibleSubstitutions
        if values.size == 1
      } yield (block, values.head)
    }

    lazy val hasSubstitutions: Boolean = substitutions.nonEmpty

    def makeSubstitutions(subs: (Vector[(Block, Int)])): Grid = {
      def makeSubstitutions(grid: Grid, subs: (Vector[(Block, Int)])): Grid = {
        if (subs.isEmpty) grid
        else {
          val (block, value) = subs.head
          makeSubstitutions(grid.updated(block, value), subs.tail)
        }
      }

      makeSubstitutions(this, subs)
    }


    override def toString: String = {
      (blocks map (_.value) grouped 9 map {
        _.mkString(" ")
      }).mkString("\n", "\n", "\n")
    }

  }


  def solve(grid: Grid): Grid = {
    def solve(grid: Grid, emptyBlocks: Vector[Block]): Grid = {
      if (grid.isValid) {
        if (grid.isSolved) grid
        else if (grid.hasSubstitutions) {
          val newGrid = grid.makeSubstitutions(grid.substitutions)
          solve(newGrid, newGrid.emptyBlocks)
        }
        else {
          //          println("********************\n\n\n\n" + grid + "\n\n\n\n*********************")
          //          throw new Exception("Can't solve")
          grid
        }
      } else {
        println("\n\n\n\n" + grid + "\n\n\n\n")
        throw new Exception("Grid no longer valid")
      }
    }

    solve(grid, grid.emptyBlocks)
  }

  val source = Source.fromFile("/Users/stefanlourens/Projects/Personal/projecteuler/resources/0096/sudoku.txt")
  val data = source.getLines().grouped(10)
  val grids: List[Grid] = (data map (Grid(_))).toList

  def answer = 0

}
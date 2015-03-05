package problems

import scala.collection.mutable
import scala.io.Source

/**
 * http://projecteuler.net/index.php?section=problems&id=96
 */
object Problem_0096 extends Problem {

  sealed trait CellChanged
  case object ValueFound extends CellChanged
  case object PossibleRemoved extends CellChanged

  class Cell(initialValue: Int, val rowIndex: Int, val columnIndex: Int, val subGridIndex: Int, val index: Int) extends mutable.Publisher[CellChanged] {
    var value = initialValue
    var possibleValues: mutable.Set[Int] = mutable.Set(1 to 9: _*)

    def isEmpty: Boolean = value == 0

    def isDefined: Boolean = !isEmpty

    def removePossible(v: Seq[Int]): Unit = v foreach removePossible

    def removePossible(v: Int): Unit = {
      if (isEmpty && possibleValues.contains(v)) {
        possibleValues remove v

        if (possibleValues.size == 1) {
          value = possibleValues.head
          possibleValues = mutable.Set()
          publish(ValueFound)
        } else publish(PossibleRemoved)


      }

    }

    override def toString: String = s"($rowIndex, $columnIndex, $subGridIndex) = $value [${possibleValues.mkString(",")}]"

  }


  class GridView(val index: Int, val cells: Seq[Cell]) {
    def this(cells: Seq[Cell]) = this(0, cells)

    def emptyCells: GridView = new GridView(index, cells filter (_.isEmpty))

    def excluding(c: Cell): GridView = new GridView(index, cells filterNot (_.index == c.index))

    def filledCells: GridView = new GridView(index, cells filterNot (_.isEmpty))

    def filledCellValues: Seq[Int] = filledCells.cells map (_.value)

    def isValid: Boolean = !(filledCells.cells groupBy (_.value) exists (_._2.size > 1))

    override def toString: String = cells mkString ","
  }

  class SubGrid(index: Int, cells: Seq[Cell]) extends GridView(index, cells)

  abstract class Lane(index: Int, cells: Seq[Cell]) extends GridView(index, cells) {
    def intersectsSubGrid(subGridIndex: Int): Boolean = cells exists (_.subGridIndex == subGridIndex)
  }

  class Row(index: Int, cells: Seq[Cell]) extends Lane(index, cells)

  class Column(index: Int, cells: Seq[Cell]) extends Lane(index, cells)

  object Grid {
    def apply(index: Int, data: Seq[String]): Grid = {
      val gridData = (data zipWithIndex).tail map { case (rowData, rowIdx) =>
        (rowData zipWithIndex) map { case (c, colIdx) =>
          new Cell(c.asDigit, rowIdx - 1, colIdx, (((rowIdx - 1) / 3) * 3) + (colIdx / 3), colIdx + ((rowIdx - 1) * 9))
        }
      }
      new Grid(index, gridData.flatten)
    }
  }

  class Grid(index: Int, cells: Seq[Cell]) extends GridView(index, cells) with mutable.Subscriber[CellChanged, mutable.Publisher[CellChanged]] {
    cells.head.subscribe(this)

    private def groupCellsBy[T <: GridView](f: Cell => Int, c: (Int, Seq[Cell]) => T): Map[Int, T] = {
      cells.groupBy(f) map { case (index, cells) => index -> c(index, cells)}
    }

    lazy val rows: Map[Int, Row] = groupCellsBy[Row](_.rowIndex, new Row(_, _))
    lazy val columns: Map[Int, Column] = groupCellsBy[Column](_.columnIndex, new Column(_, _))
    lazy val subGrids: Map[Int, SubGrid] = groupCellsBy[SubGrid](_.subGridIndex, new SubGrid(_, _))

    override def notify(pub: mutable.Publisher[CellChanged], event: CellChanged): Unit = {
      pub.asInstanceOf

    }

    override def toString: String = {
      "Grid " + index + ":" + (cells map (_.value) grouped 9 map {
        _.mkString(" ")
      }).mkString("\n", "\n", "\n")
    }


  }


  //
  //  case class Grid(name: String, blocks: Vector[Block], cache: Map[Block, List[Int]]) extends GridArea {
  //    var pvCache: Map[Block, List[Int]] =
  //      if (cache.isEmpty) Map() withDefault calcNonEliminatedValues
  //      else cache
  //
  //    def updated(b: Block, value: Int): Grid = new Grid(name, blocks.updated(b.index, b.updated(value)), pvCache)
  //
  //    def findBlocks(p: Block => Boolean): Vector[Block] = blocks filter p
  //
  //    def findBlock(index: Int): Block = (blocks find (_.index == index)).get
  //
  //    def findBlock(row: Int, column: Int): Block = (blocks find (b => b.row == row && b.column == column)).get
  //
  //    lazy val rows: IndexedSeq[Row] =
  //      0 until 9 map (row => new Row(row, findBlocks(_.row == row)))
  //
  //    lazy val columns: IndexedSeq[Column] =
  //      0 until 9 map (col => new Column(col, findBlocks(_.column == col)))
  //
  //    lazy val subGrids: IndexedSeq[SubGrid] =
  //      0 until 9 map (subGrid => new SubGrid(subGrid, findBlocks(_.subGrid == subGrid)))
  //
  //    def siblingCols(b: Block): IndexedSeq[Column] =
  //      columns filter { column => column.num != b.column && column.intersectsSubGrid(b.subGrid)}
  //
  //    def siblingRows(b: Block): IndexedSeq[Row] =
  //      rows filter { row => row.num != b.row && row.intersectsSubGrid(b.subGrid)}
  //
  //    def containsAllNumbers(gridAreas: IndexedSeq[GridArea]): Boolean = gridAreas forall containsAllNumbers
  //
  //    def containsAllNumbers(gridArea: GridArea): Boolean = {
  //      val values = (gridArea.blocks map (_.value)).toSet
  //      1 to 9 forall (values contains)
  //    }
  //
  //    def eliminations(b: Block): Set[Int] = (rows(b.row).blocks ++ columns(b.column).blocks
  //      ++ subGrids(b.subGrid).blocks filterNot (_.isEmpty) map (_.value)).toSet
  //
  //    def calcNonEliminatedValues(b: Block): List[Int] = {
  //      (1 to 9 filterNot (eliminations(b) contains)).toList
  //    }
  //
  //    def findBlocksWithPossibleValue(v: Int, blocks: Vector[Block]): Vector[Block] = {
  //      blocks filter (calcNonEliminatedValues(_).contains(v))
  //    }
  //
  //    def calcPossibleValues(b: Block): List[Int] = {
  //      pvCache = pvCache.updated(b, pvCache(b) filter calcNonEliminatedValues(b).contains)
  //      val possibleValues = pvCache(b)
  //
  //      //Check if one if the possible values is the only one in the row or col or grid (Pinned Squares)
  //      val rowPossibles = (rows(b.row).emptyBlocksExcluding(b) flatMap pvCache).toSet
  //      val colPossibles = (columns(b.column).emptyBlocksExcluding(b) flatMap pvCache).toSet
  //      val gridPossibles = (subGrids(b.subGrid).emptyBlocksExcluding(b) flatMap pvCache).toSet
  //
  //
  //      val pinnedValue =
  //        possibleValues find (pv => !rowPossibles.contains(pv) || !colPossibles.contains(pv) || !gridPossibles.contains(pv))
  //
  //      /*
  //        Intersection Removal:
  //          Check if any grid the row/col intersects only has suggestions for a number in that particular row/col
  //      */
  //      val intersectionExcludesForRow = for {
  //        pv <- possibleValues
  //        (subGrid, blocks) <- rows(b.row).emptyBlocksExcluding(_.subGrid == b.subGrid) groupBy (_.subGrid)
  //        blocksWithSuggestion = findBlocksWithPossibleValue(pv, blocks)
  //        if blocksWithSuggestion.nonEmpty && findBlocksWithPossibleValue(pv, subGrids(subGrid).emptyBlocksExcluding(blocks)).isEmpty
  //      } yield pv
  //
  //      val intersectionExcludesForCol = for {
  //        pv <- possibleValues
  //        (subGrid, blocks) <- columns(b.column).emptyBlocksExcluding(_.subGrid == b.subGrid) groupBy (_.subGrid)
  //        blocksWithSuggestion = findBlocksWithPossibleValue(pv, blocks)
  //        if blocksWithSuggestion.nonEmpty && findBlocksWithPossibleValue(pv, subGrids(subGrid).emptyBlocksExcluding(blocks)).isEmpty
  //      } yield pv
  //
  //      val intersectionExcludes = intersectionExcludesForCol ++ intersectionExcludesForRow
  //
  //      /*
  //        Pointing Pairs
  //      */
  //      val siblingRowGridExcludes =
  //        for {
  //          pv <- possibleValues
  //          rowEmpties <- siblingRows(b) map (_.emptyBlocks)
  //          blocksWithValue = findBlocksWithPossibleValue(pv, rowEmpties)
  //          if blocksWithValue.nonEmpty && blocksWithValue.forall(_.subGrid == b.subGrid)
  //        } yield pv
  //
  //
  //      val siblingColGridExcludes =
  //        for {
  //          pv <- possibleValues
  //          colEmpties <- siblingCols(b) map (_.emptyBlocks)
  //          blocksWithValue = findBlocksWithPossibleValue(pv, colEmpties)
  //
  //          if blocksWithValue.nonEmpty && blocksWithValue.forall(_.subGrid == b.subGrid)
  //        } yield pv
  //
  //      val siblingGridExcludes = siblingRowGridExcludes ++ siblingColGridExcludes
  //
  //
  //      /*
  //        Naked subsets
  //       */
  //      val gridNakedSubsets = {
  //        val mappedPVS: Map[List[Int], Vector[Block]] = subGrids(b.subGrid).emptyBlocksExcluding(b) groupBy pvCache
  //        (mappedPVS filter{ case (pvs, blocks) => pvs.size == blocks.size}).keySet.flatten.toSet.toList
  //      }
  //
  //      val rowNakedSubsets = {
  //        val mappedPVS: Map[List[Int], Vector[Block]] = rows(b.row).emptyBlocksExcluding(b) groupBy pvCache
  //        (mappedPVS filter{ case (pvs, blocks) => pvs.size == blocks.size}).keySet.flatten.toSet.toList
  //      }
  //
  //      val colNakedSubsets = {
  //        val mappedPVS: Map[List[Int], Vector[Block]] = columns(b.column).emptyBlocksExcluding(b) groupBy pvCache
  //        (mappedPVS filter{ case (pvs, blocks) => pvs.size == blocks.size}).keySet.flatten.toSet.toList
  //      }
  //
  //      val subSetExcludes = gridNakedSubsets ++ rowNakedSubsets ++ colNakedSubsets
  //
  //      //Hidden pairs
  ////        val hiddenPairExcludes = {
  ////        val possibleExcludes = for {
  ////          empties <- List(rows(b.row), columns(b.column), subGrids(b.subGrid)) map(_.emptyBlocksExcluding(b))
  ////          pv <- possibleValues
  ////          if findBlocksWithPossibleValue(pv, empties).size == 1
  ////        } yield pv
  ////
  ////        if (possibleExcludes > 1)
  ////      }
  //
  //      /*
  //        X-Wing
  //      */
  //      val xWingColExcludes = for {
  //        pv <- possibleValues
  //        cellsInFirstColWithPV = findBlocksWithPossibleValue(pv, columns(b.column).emptyBlocksExcluding(b))
  //        if cellsInFirstColWithPV.nonEmpty
  //        firstColCell <- cellsInFirstColWithPV
  //        cellsInFirstRowWithPv = findBlocksWithPossibleValue(pv, rows(firstColCell.row).emptyBlocksExcluding(firstColCell))
  //        if cellsInFirstRowWithPv.size == 1
  //        firstRowCell = cellsInFirstRowWithPv(0)
  //        cellsInSecondColWithPV = findBlocksWithPossibleValue(pv, columns(firstRowCell.column).emptyBlocksExcluding(firstRowCell))
  //        if cellsInSecondColWithPV.nonEmpty
  //        secondColCell <- cellsInSecondColWithPV
  //        cellsInSecondRowWithPv = findBlocksWithPossibleValue(pv, rows(secondColCell.row).emptyBlocksExcluding(secondColCell))
  //        if cellsInSecondRowWithPv.size == 1
  //        secondRowCell = cellsInSecondRowWithPv(0)
  //        if cellsInFirstColWithPV exists(_.row == secondColCell.row)
  //      } yield pv
  //
  //      if (xWingColExcludes.nonEmpty) {
  //        println("Found xWingColExcludes (" + xWingColExcludes.mkString(",") + ") for block = " + b)
  //      }
  //
  //
  //      val xWingRowExcludes = for {
  //        pv <- possibleValues
  //        cellsInFirstRowWithPV = findBlocksWithPossibleValue(pv, rows(b.row).emptyBlocksExcluding(b))
  //        if cellsInFirstRowWithPV.nonEmpty
  //        firstRowCell <- cellsInFirstRowWithPV
  //        cellsInFirstColWithPv = findBlocksWithPossibleValue(pv, columns(firstRowCell.column).emptyBlocksExcluding(firstRowCell))
  //        if cellsInFirstColWithPv.size == 1
  //        firstColCell = cellsInFirstColWithPv(0)
  //        cellsInSecondRowWithPV = findBlocksWithPossibleValue(pv, rows(firstColCell.row).emptyBlocksExcluding(firstColCell))
  //        if cellsInSecondRowWithPV.nonEmpty
  //        secondRowCell <- cellsInSecondRowWithPV
  //        cellsInSecondColWithPv = findBlocksWithPossibleValue(pv, columns(secondRowCell.column).emptyBlocksExcluding(secondRowCell))
  //        if cellsInSecondColWithPv.size == 1
  //        secondColCell = cellsInSecondColWithPv(0)
  //        if cellsInFirstRowWithPV exists(_.column == secondRowCell.column)
  //      } yield pv
  //
  //      if (xWingRowExcludes.nonEmpty) {
  //        println("Found xWingRowExcludes (" + xWingRowExcludes.mkString(",") + ") for block = " + b)
  //      }
  //
  //
  //      val exclusions: List[Int]x = intersectionExcludes ++ siblingGridExcludes ++ subSetExcludes ++ xWingColExcludes ++ xWingRowExcludes
  //
  //      if (pinnedValue.isDefined) List(pinnedValue.get)
  //      else {
  //        val newPossibles = possibleValues filterNot exclusions.contains
  //        pvCache.updated(b, newPossibles)
  //        pvCache(b)
  //      }
  //    }
  //
  //    lazy val possibleSubstitutions: Vector[(Block, List[Int])] = findBlocks(_.isEmpty) map (b => (b, calcPossibleValues(b)))
  //
  //    lazy val isNotSolvable: Boolean = possibleSubstitutions exists { case (b, pv) => b.isEmpty && pv.isEmpty}
  //
  //    lazy val isSolvable: Boolean = !isNotSolvable
  //
  //    lazy val isSolved: Boolean = emptyBlocks.isEmpty && List(rows, columns, subGrids).forall(containsAllNumbers)
  //
  //    override lazy val isValid: Boolean = rows ++ columns ++ subGrids forall (_.isValid)
  //
  //    lazy val substitutions: Vector[(Block, Int)] = {
  //      for {
  //        (block, values) <- possibleSubstitutions
  //        if values.size == 1
  //      } yield (block, values.head)
  //    }
  //
  //    lazy val hasSubstitutions: Boolean = substitutions.nonEmpty
  //
  //    def makeSubstitutions(subs: (Vector[(Block, Int)])): Grid = {
  //      def makeSubstitutions(grid: Grid, subs: (Vector[(Block, Int)])): Grid = {
  //        if (subs.isEmpty) grid
  //        else {
  //          val (block, value) = subs.head
  //          makeSubstitutions(grid.updated(block, value), subs.tail)
  //        }
  //      }
  //
  //      makeSubstitutions(this, subs)
  //    }
  //
  //
  //    override def toString: String = {
  //      name + ":" + (blocks map (_.value) grouped 9 map {
  //        _.mkString(" ")
  //      }).mkString("\n", "\n", "\n")
  //    }
  //
  //  }
  //
  //
  //  def solve(grid: Grid): Grid = {
  //    def solve(grid: Grid, emptyBlocks: Vector[Block]): Grid = {
  //      if (grid.isValid) {
  //        if (grid.isSolved) grid
  //        else if (grid.hasSubstitutions) {
  //          val newGrid = grid.makeSubstitutions(grid.substitutions)
  //          solve(newGrid, newGrid.emptyBlocks)
  //        }
  //        else {
  //          //          println("********************\n\n\n\n" + grid + "\n\n\n\n*********************")
  //          //          throw new Exception("Can't solve")
  //          grid
  //        }
  //      } else {
  //        println("\n\n\n\n" + grid + "\n\n\n\n")
  //        throw new Exception("Grid no longer valid")
  //      }
  //    }
  //
  //    solve(grid, grid.emptyBlocks)
  //  }

  val source = Source.fromFile("/Users/stefanlourens/Projects/Personal/projecteuler/resources/0096/sudoku.txt")
  val data = source.getLines().grouped(10).zipWithIndex
  val grids: List[Grid] = (data map { case (cells, index) => Grid(index, cells) }).toList

  def answer = 0

}
package problems

/**
 * http://projecteuler.net/problem=18
 *
 * Find the maximum total from top to bottom of the triangle below
 */
object Problem_0018 extends Problem {

  val triangle =
    """
	3
	7 4
	2 4 6
	8 5 9 3
    """


  object Tree {
  	def apply() = EmptyTree()
  	def apply(value: Int, left: Tree, right: Tree) = new NonEmptyTree(value, left, right)
  }
  sealed trait Tree
  case class NonEmptyTree(value: Int, left: Tree, right: Tree) extends Tree
  case class EmptyTree extends Tree


  Tree(0, Tree(), Tree())


  def answer = ???//pathCountFrom(0, grid.origin)new Numeral(100)

}
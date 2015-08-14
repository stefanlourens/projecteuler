package problems

import scala.io.Source

/**
 * http://projecteuler.net/problem=107
 *
 * Find the maximum saving which can be achieved by removing redundant edges whilst ensuring that the
 * network remains connected.
 */
object Problem_0107 extends Problem {

  case class Node(num: Int)

  case class Connection(a: Node, b: Node)(val weight: Int) {
    def contains(n: Node): Boolean = a == n || b == n

    def notIn(nodes: List[Node]): Node = {
      if (nodes contains a) b
      else if (nodes contains b) a
      else throw new Exception("Node not found")
    }

  }

  val source = Source.fromFile("resources/0107/p107_network.txt")

  val lines: List[String] = source.getLines().toList

  val connections: List[Connection] = {
    lines.zipWithIndex flatMap { case (line, sourceIdx) =>
      line.split(",").zipWithIndex.drop(sourceIdx) flatMap { case (weight: String, destIndex) =>
        if (weight != "-") Some(Connection(Node(sourceIdx), Node(destIndex))(weight.toInt))
        else None
      }
    }
  }.sortBy(-_.weight)


  def answer = {
    def isConnected(a: Node, b: Node, connections: List[Connection]): Boolean = {
      def isConnected(a: List[Node], b: Node, connections: List[Connection]): Boolean = {
        if (connections.isEmpty) false
        else {
          val connectionsContainingA = connections.filter { c => a.exists(n => c.contains(n)) }

          if (connectionsContainingA.isEmpty) false
          else if (connectionsContainingA.exists(_.contains(b))) true
          else {
            val siblingNodes = connectionsContainingA map {
              _.notIn(a)
            }
            isConnected(siblingNodes, b, connections diff connectionsContainingA)
          }
        }
      }

      isConnected(List(a), b, connections)
    }

    def isRemovable(connection: Connection, connections: List[Connection]): Boolean = {
      isConnected(connection.a, connection.b, connections diff List(connection))
    }

    def optimize(connections: List[Connection]): List[Connection] = {
      def optimize(connections: List[Connection], acc: List[Connection]): List[Connection] = {
        if (connections.isEmpty) acc
        else if (isRemovable(connections.head, connections.tail ++ acc)) optimize(connections.tail, acc)
        else optimize(connections.tail, connections.head :: acc)
      }

      optimize(connections, List())
    }

    val optimized = optimize(connections)
    val origWeight = (connections map (_.weight)).sum
    val optimizedWeight = (optimized map (_.weight)).sum

    origWeight - optimizedWeight
  }
}
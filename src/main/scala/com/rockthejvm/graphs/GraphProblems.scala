package com.rockthejvm.graphs

import scala.annotation.tailrec

object GraphProblems {

  type Graph[T] = Map[T, Set[T]]

  val socialNetwork: Graph[String] = Map(
    "Alice" -> Set("Bob", "Charlie", "David"),
    "Bob" -> Set(),
    "Charlie" -> Set("David"),
    "David" -> Set("Bob", "Mary"),
    "Mary" -> Set("Bob", "Charlie")
  )

  /**
   * Easy problems
   */
  // number of nodes this `node` is associated with (adjacent to)
  def outDegree[T](graph: Graph[T], node: T): Int =
    graph.get(node).map(_.size).getOrElse(0)

  // number of nodes connected to `node`
  def inDegree[T](graph: Graph[T], node: T): Int =
    graph.count(_._2.contains(node))

  /**
   * Medium problems
   */
  // confirm if a path exists in a graph between two nodes
  def isPath[T](graph: Graph[T], start: T, end: T): Boolean = {
    /*
    Alice -> Mary
    = ipt([Alice], [])
    = ipt([Bob, Charlie, David], [Alice])
    = ipt([Charlie, David], [Bob, Alice])
    = ipt([David], [Charlie, Bob, Alice])
    = ipt([Mary], [David, Charlie, Bob, Alice])
    = true
    */
    @tailrec
    def searchTailrec(nodesToCheck: Set[T], nodesChecked: Set[T]): Boolean = {
      if (nodesToCheck.isEmpty) false
      else {
        val node = nodesToCheck.head
        val connections = graph(node)
        if (connections.contains(end)) true
        else searchTailrec(nodesToCheck - node ++ connections -- nodesChecked, nodesChecked + node)
      }
    }

    if (!graph.contains(start) || !graph.contains(end)) false
    else searchTailrec(Set(start), Set())
  }

  def main(args: Array[String]): Unit = {
    def testEasyProblems() = {
      println(outDegree(socialNetwork, "Alice")) // 3
      println(inDegree(socialNetwork, "David")) // 2
    }

    def testMediumProblems() = {
    println(isPath(socialNetwork, "Alice", "Mary")) // true
    println(isPath(socialNetwork, "Bob", "Charlie")) // false
    }

    testMediumProblems()
  }
}
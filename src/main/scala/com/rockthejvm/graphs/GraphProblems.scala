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
  // number of nodes this `node` is associated (adjacent) to
  def outDegree[T](graph: Graph[T], node: T): Int =
    graph.get(node).map(_.size).getOrElse(0)

  // number of nodes connected no `node`
  def inDegree[T](graph: Graph[T], node: T): Int =
    graph.count(_._2.contains(node))

  def main(args: Array[String]): Unit = {
    def testEasyProblems() = {
      println(outDegree(socialNetwork, "Alice")) // 3
      println(inDegree(socialNetwork, "David")) // 2
    }

    testEasyProblems()
  }
}
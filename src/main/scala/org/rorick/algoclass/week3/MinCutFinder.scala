package org.rorick.algoclass.week3


/**
 * Implementation of randomized min cut search algorithm.
 */
class MinCutFinder(incidents: List[List[Int]], nextNode: (Int => Int)) {
  def this(incidents: List[List[Int]]) = this(incidents, _ => 0)

  lazy val minCutSize = calculateMinCutSize(new Graph(incidents))


  private def calculateMinCutSize(graph: Graph): Int = {

    // divide by two because graph is undirected and has edges in both directions
    if (graph.nodes.size > 2) {
      // pick random edge
      graph.contractNode(nextNode(graph.edges.size))
      // call recursively
      calculateMinCutSize(graph)
    } else {
      println(graph.edges)
      graph.edges.size / 2
    }
  }
}

sealed trait Node

case class SimpleNode(value: Int) extends Node

case class MergedNode(values: Node*) extends Node


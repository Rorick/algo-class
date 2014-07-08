package org.rorick.algoclass.part1.week3

import scala.collection.mutable.ListBuffer

/**
 * Mutable grap for min cut problem.
 */
class Graph(incidents: List[List[Int]]) {
  //  val random: Random

  import org.rorick.algoclass.part1.week3.Graph.Edge

  val nodes: ListBuffer[Node] = ListBuffer()
  val edges: ListBuffer[Edge] = ListBuffer()

  def incidentals(n: Node): List[Edge] = {
    edges filter {
      case (u, v) => n == u || n == v
    }
  }.toList

  for (value :: incidents <- incidents) {
    val node = SimpleNode(value)
    nodes += node
    val edges = incidents map (v => (node, SimpleNode(v)))
    this.edges ++= edges
  }

  assert(edges.size % 2 == 0)

  def contractNode(edgeNum: Int) {
    val (u, v) = edges(edgeNum)
    val merged = new MergedNode(u, v)
    nodes -= u
    nodes -= v
    nodes += merged
    val newEdges = new ListBuffer[Edge]
    edges.foreach {
      case e@(a, b) if e ==(u, v) || e ==(v, u) =>
      case (a, b) if a == u || a == v => newEdges += ((merged, b))
      case (a, b) if b == u || b == v => newEdges += ((a, merged))
      case e => newEdges += e
    }
    edges.clear()
    edges ++= newEdges
  }

}

object Graph {
  type Edge = (Node, Node)
}

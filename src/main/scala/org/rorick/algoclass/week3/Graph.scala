package org.rorick.algoclass.week3

import collection.mutable.ListBuffer
import collection.mutable.Map

/**
 * Mutable grap for min cut problem.
 */
class Graph(incidents: List[List[Int]]) {
//  val random: Random
  import Graph.Edge

  val nodes: ListBuffer[Node]= ListBuffer()
  val edges: ListBuffer[Edge] = ListBuffer()
  val incidentals: Map[Node, List[Edge]] = Map()

  for (value :: incidents  <- incidents) {
    val node = SimpleNode(value)
    nodes += node
    val edges = incidents map (v => (node, SimpleNode(v)))
    this.edges ++= edges
    incidentals(node) = edges
  }
}

object Graph {
  type Edge = (Node, Node)
}

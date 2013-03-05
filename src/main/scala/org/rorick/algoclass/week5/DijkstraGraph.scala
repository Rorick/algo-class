package org.rorick.algoclass.week5

import collection.mutable.ListBuffer
import collection.mutable

/**
 * Graph for solving assignment.
 */
class DijkstraGraph {
  private val ns = ListBuffer[Node]()
  private val es = ListBuffer[(Edge, Int)]()
  private val incs = mutable.Map[Node, ListBuffer[(Edge, Int)]]().withDefault(_ => ListBuffer())

  def addNode(node: Node, edgesInfo: List[(Node, Int)]) {
    require(1 to 200 contains node)

    ns += node
    edgesInfo foreach {
      case (n, l) =>
        require(1 to 200 contains n)
        require(0 <= l && l <= 1000000)

        val edge = (node -> n, l)
        es += edge
        val incidents = incs(node)
        incidents += edge
        incs(node) = incidents
    }
  }

  def nodes: List[Node] = ns.toList

  def edges: List[(Edge, Int)] = es.toList

  def incidents(node: Node): List[(Edge, Int)] = incs(node).toList

  override def toString = s"Undirected graph[#nodes=${nodes.length}, #edges=${edges.length}]"
}

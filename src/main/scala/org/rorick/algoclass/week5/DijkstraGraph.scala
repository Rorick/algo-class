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
        require(0 <= l && l <= DijkstraGraph.Infinity)

        val edge = (node -> n, l)
        es += edge
        val incidents = incs(node)
        incidents += edge
        incs(node) = incidents
    }
  }

  def shortestPathsDistances(source: Node): Map[Node, Int] = {
    val distances = mutable.Map(source -> 0).withDefaultValue(DijkstraGraph.Infinity)
    val X = mutable.Set(source)
    val V = mutable.Set() ++ nodes - source

    while (!V.isEmpty) {
      val ((_, v), bestLen) = {
        for {
          ((u, v), l) <- edges
          if X(u) && V(v)
        } yield (u -> v, distances(u) + l)
      } minBy {
        case (_, l) => l
      }
      X += v
      V -= v
      distances(v) = bestLen
    }

    distances.toMap
  }

  def nodes: List[Node] = ns.toList

  def edges: List[(Edge, Int)] = es.toList

  def incidents(node: Node): List[(Edge, Int)] = incs(node).toList

  override def toString = s"Undirected graph[#nodes=${nodes.length}, #edges=${edges.length}]"
}

object DijkstraGraph {
  val Infinity = 1000000
}


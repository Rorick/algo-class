package org.rorick.algoclass.part1.week5

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * Graph for solving assignment.
 */
class DijkstraGraph {
  private val ns = ListBuffer[Node]()
  private val es = ListBuffer[(Edge, Int)]()
  private val incs = mutable.Map[Node, ListBuffer[(Edge, Int)]]().withDefaultValue(ListBuffer.empty)

  def addNode(node: Node, edgesInfo: List[(Node, Int)]) {
    require(DijkstraGraph.AllowedNodes contains node)

    ns += node
    edgesInfo foreach {
      case (n, l) =>
        require(DijkstraGraph.AllowedNodes contains n)
        require(0 <= l && l <= DijkstraGraph.Infinity)

        val edge = (node -> n, l)
        es += edge
        val incidents = incs.getOrElseUpdate(node, ListBuffer())
        incidents += edge
    }
  }

  def shortestPathsDistances(source: Node): Map[Node, Int] = {
    val distances = mutable.Map(source -> 0).withDefaultValue(DijkstraGraph.Infinity)
    val X = mutable.Set(source)
    val V = mutable.Set() ++ nodes - source

    while (V.nonEmpty) {
      val (v, bestLen) = {
        for {
          ((u, v), l) <- edges
          if X(u) && V(v)
        } yield (v, distances(u) + l)
      } minBy {
        case (_, l) => l
      }
      X += v
      V -= v
      distances(v) = bestLen
    }

    distances.toMap
  }

  def shortestPathsDistancesWithHeap(source: Node): Map[Node, Int] = {
    val distances = mutable.Map(source -> 0).withDefaultValue(DijkstraGraph.Infinity)
    val X: mutable.Set[Node] = mutable.Set()
    val heapContent = List((0, source)) ++
      List.fill(nodes.size - 1)(DijkstraGraph.Infinity).zip(Set(nodes: _*) - source)
    // reverse ordering by first element, i.e. distance
    // heap contains the next node to extract with shortest distance from source
    var heap = mutable.PriorityQueue(heapContent: _*)(Ordering.Tuple2(Ordering.Int.reverse, Ordering.Int))

    while (heap.nonEmpty) {
      val (bestLen, v) = heap.dequeue()
      X += v
      distances(v) = bestLen
      val incidentEdges = incidents(v)
      // modify heap. As remove is not supported, simulate it
      val toUpdate = for {((_, u), l) <- incidentEdges if !X(u)} yield ((distances(u), u), l)
      heap = heap.filterNot {
        toUpdate.map(_._1).contains(_)
      }
      for (((curBestLen, u), l) <- toUpdate) {
        assert(curBestLen == distances(u))
        val newBestLen = math.min(curBestLen, bestLen + l)
        heap.enqueue((newBestLen, u))
        distances(u) = newBestLen
      }
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
  val AllowedNodes = 1 to 200
}


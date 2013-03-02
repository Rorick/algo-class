package org.rorick.algoclass.week4

import collection.mutable
import collection.mutable.{ArrayBuffer, ListBuffer}

/**
 * Graph representation for SCCs problem. Would be great if we could unify it with week 3 graph, but that is completely
 * optional.
 */
class Graph(val edges: ArrayBuffer[Edge]) {
  def this() = this(ArrayBuffer[Edge]())

  def this(edges: Edge*) = this(ArrayBuffer[Edge](edges: _*))

  def nodes = {
    val ns = collection.mutable.Set[Int]()
    edges.foreach {
      e =>
        ns += e._1
        ns += e._2
    }
    ns.toVector.sorted
  }


  def reversed = new Graph(edges map {
    case (u, v) => (v, u)
  })

  def sccs: List[Int] = {
    // reverse edges
    val gRev = reversed
    // run DFS-loop calculating finishing times

    val fs = Graph.dfsLoopF(gRev)

    // restore edges
    // run DFS-loop from top finishing time downwards calculating lead nodes
    val sccs: List[Int] = dfsLoopScc(fs)
    // return list of nodes grouped by lead node

    //    sccs.view.sorted(Ordering[Int].reverse).take(5).force
    sccs
  }

  def dfsLoopScc(finishingTimes: mutable.Map[Int, Int]): List[Int] = {
    val newLabels = finishingTimes.toList.map {
      case (u, v) => (v, u)
    }.toMap


    val explored = collection.mutable.Set[Int]()
    val sccsSizes = ListBuffer[Int]()
    newLabels.size to 1 by -1 foreach {
      ft =>
        val node = newLabels(ft)
        val visited = collection.mutable.Set[Int]()
        if (!explored(node)) {
          dfs1(this, node, explored, visited)
          sccsSizes += visited.size
        }

    }
    sccsSizes.toList
  }

  def dfs1(graph: Graph, node: Int, explored: mutable.Set[Int], visited: mutable.Set[Int]) {
    val stack = mutable.Stack[(Int, Seq[Edge])]()
    stack.push((node, graph.incidents(node)))
    while (!stack.isEmpty) {

      val (n, edges) = stack.pop()

      if (!explored(n)) {
        explored += n
        visited += n
      }

      val unexploredEdges = edges.filterNot({
        case (_, v) => explored(v)
      })
      if (unexploredEdges.isEmpty) {
      } else {
        val (_, v) = unexploredEdges(0)
        val restUnexploredEdges = unexploredEdges.drop(1)
        stack.push((n, restUnexploredEdges))
        stack.push((v, graph.incidents(v)))
      }
    }
  }


  val incidents = {
    val incidents = mutable.Map[Int, ArrayBuffer[Edge]]().withDefault(_  => ArrayBuffer[Edge]())
    edges foreach {
      case e@(u, _) => {
        val inc = incidents(u)
        inc += e
        incidents(u) = inc
      }
    }
    incidents
  }
}

object Graph {
  // fixme: this is a piece of crap!
  private var t = 0

  def dfsLoopF(g: Graph): mutable.Map[Int, Int] = {
    t = 0
    val fs: mutable.Map[Int, Int] = mutable.Map.empty
    // for each node starting from highest
    val nodes = g.nodes
    val explored = collection.mutable.Set[Int]()
    nodes.size - 1 to 0 by -1 foreach {
      n =>
      //   if node is not yet explored
        if (!explored(nodes(n))) {
          //     DFS(g, node, explored)
          dfs(g, nodes(n), explored, fs)
        }
    }
    fs
  }

  def dfs(graph: Graph, node: Int, explored: mutable.Set[Int], fs: mutable.Map[Int, Int]) {
    val stack = mutable.Stack[(Int, Seq[Edge])]()
    stack.push((node, graph.incidents(node)))
    while (!stack.isEmpty) {

      val (n, edges) = stack.pop()

      if (!explored(n)) {
        explored += n
      }

      val unexploredEdges = edges.filterNot({
        case (_, v) => explored(v)
      })
      if (unexploredEdges.isEmpty) {
        t = t + 1
        fs(n) = t
      } else {
        val (_, v) = unexploredEdges(0)
        val restUnexploredEdges = unexploredEdges.drop(1)
        stack.push((n, restUnexploredEdges))
        stack.push((v, graph.incidents(v)))
      }
    }
  }

}

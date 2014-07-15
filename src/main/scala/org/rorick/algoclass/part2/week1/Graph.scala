package org.rorick.algoclass.part2.week1

import scala.collection.generic.Growable
import scala.collection.mutable

/**
 * Graph for solving Minimum Spanning Tree problem.
 */
class Graph extends Growable[Edge] {

  private[this] val ns = mutable.Set[Node]()
  private[this] val es = mutable.Set[Edge]()
  private[this] val incs = mutable.Map[Node, mutable.Set[Edge]]().withDefaultValue(mutable.Set.empty)

  def minimumSpanningTree(): Set[Edge] = {
    // initialize X = {s}, s any node from V = nodes
    // T = empty set, X = vertices spanned so far by tree T
    val nodeList = nodes.toList
    val T = mutable.Set[Edge]()
    if (nodeList.nonEmpty) {
      val X = mutable.Set(nodeList.head)
      val V = mutable.Set(nodeList.tail: _*)
      // while X != V
      while (V.nonEmpty) {
        //  let e = (u, v) be the cheapest edge of Graph with u from X and v from V - X
        val e = cheapestEdge(X, V)
        //  add e to T
        T += e
        //  add v to X
        val v = if (V(e.v)) e.v else e.u
        X += v
        V -= v
      }
    }
    T.toSet
  }

  def cheapestEdge(X: mutable.Set[Node], V: mutable.Set[Node]): Edge = {
    for {
      e <- edges
      if e.crossesCut(X, V)
    } yield e
  }.minBy(_.l)

  def +=(u: Node, v: Node, l: Int): this.type = {
    this += (if (u < v) Edge(u, v, l) else Edge(v, u, l))
  }

  def +=(edge: Edge): this.type = {
    ns +=(edge.u, edge.v)
    es += edge
    val incidents = incs.getOrElseUpdate(edge.u, mutable.Set())
    incidents += edge
    this
  }

  override def clear(): Unit = throw new UnsupportedOperationException("Operation is not supported.")

  def nodes: Set[Node] = ns.toSet

  def edges: Set[Edge] = es.toSet

  def incidents(node: Node): Set[Edge] = incs(node).toSet

  def size = ns.size
}

object Graph {
  def apply(edges: Edge*): Graph = new Graph ++= edges
}

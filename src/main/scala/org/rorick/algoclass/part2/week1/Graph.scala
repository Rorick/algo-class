package org.rorick.algoclass.part2.week1

import scala.collection.generic.Growable
import scala.collection.mutable

/**
 * Graph for solving Minimum Spanning Tree problem.
 */
class Graph extends Growable[Edge] {
  private[this] val ns = mutable.Set[Node]()
  private[this] val es = mutable.Set[Edge]()
  private[this] val incsOut = mutable.Map[Node, mutable.Set[Edge]]().withDefaultValue(mutable.Set.empty)
  private[this] val incsIn = mutable.Map[Node, mutable.Set[Edge]]().withDefaultValue(mutable.Set.empty)

  def addEdge(edge: Edge): this.type = {
    ns += (edge.u, edge.v)
    es += edge
    val incidentsOut = incsOut.getOrElseUpdate(edge.u, mutable.Set())
    incidentsOut += edge
    val incidentsIn = incsIn.getOrElseUpdate(edge.v, mutable.Set())
    incidentsIn += edge
    this
  }

  def minimumSpanningTree(): Set[Edge] = {
    // initialize X = {s}, s any node from V = nodes
    // T = empty set, X = vertices spanned so far by tree T
    // while X != V
    //  let e = (u, v) be the cheapest edge of Graph with u from X and v from V - X
    //  add e to T
    //  add v to X
    Set.empty
  }

  def += (edge: Edge): this.type = addEdge(edge)

  override def clear(): Unit = throw new UnsupportedOperationException("Operation is not supported.")

  def nodes: Set[Node] = ns.toSet
  
  def edges: Set[Edge] = es.toSet
  
  def incidentsOut(node: Node): Set[Edge] = incsOut(node).toSet

  def incidentsIn(node: Node): Set[Edge] = incsIn(node).toSet
}

object Graph {
  def apply(edges: Edge*): Graph = new Graph ++= edges
}

package org.rorick.algoclass.week3


/**
 * Implementation of randomized min cut search algorithm.
 */
class MinCutFinder(incidents: List[List[Int]]) {

  lazy val minCutSize = calculateMinCutSize(new Graph(incidents))


  private def calculateMinCutSize(graph: Graph): Int = {

    //    if (incidents.size > 1) 1 else 0
    // divide by two because graph is undirected and has edges in both directions
    if (graph.nodes.size > 2) {
      // pick random edge
      graph.contractNode(0)
      /*val edge = graph.edges(0)
      // contract it
      // update nodes
      graph.nodes -= edge._1
      graph.nodes -= edge._2
      val superNode: Node = new MergedNode(edge._1, edge._2)
      graph.nodes += superNode
      // update edges
      graph.edges -= ((edge._1, edge._2))
      graph.edges -= ((edge._2, edge._1))
      graph.edges ++= graph.incidentals(edge._1).filter(_ !=(edge._1, edge._2)).map{case (n1, n2) => (superNode, n2)}
      graph.edges ++= graph.incidentals(edge._2).filter(_ !=(edge._2, edge._1)).map{case (n1, n2) => (superNode, n2)}
      // update incidents
      val newIncidentals = (
        graph.incidentals(edge._1).filter(_ !=(edge._1, edge._2))
          union
          graph.incidentals(edge._2).filter(_ !=(edge._2, edge._1)))

      graph.incidentals.remove(edge._1)
      graph.incidentals.remove(edge._2)
      graph.incidentals(superNode) = newIncidentals

      // remove self loops*/
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


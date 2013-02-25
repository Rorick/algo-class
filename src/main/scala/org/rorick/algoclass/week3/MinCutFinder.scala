package org.rorick.algoclass.week3

import util.Random
import io.Source
import collection.mutable.ListBuffer


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
      graph.edges.size / 2
    }
  }
}

object MinCutFinder extends App {

  val graph = new ListBuffer[List[Int]]
  Source.fromFile("C:\\Users\\Rorick\\IdeaProjects\\algo-class\\src\\main\\resources\\kargerMinCut.txt").getLines().foreach {
    line =>
      graph += line.split("\t").map(_.toInt).toList
  }

  val numNodes = graph.size
  val numTrials = Math.ceil(numNodes * 2 * Math.log(numNodes)).toInt
  val minCuts = new ListBuffer[Int]

  (1 to numTrials) foreach {
    i =>
      val random = new Random
      minCuts += new MinCutFinder(graph.toList, n => random.nextInt(n)).minCutSize
  }
  println(minCuts.min)
}
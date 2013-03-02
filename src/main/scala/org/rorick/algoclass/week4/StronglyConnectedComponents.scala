package org.rorick.algoclass.week4

import io.Source
import collection.mutable.ArrayBuffer

/**
 * Solution for programming assignment week 4.
 */
object StronglyConnectedComponents extends App {
  val g = new ArrayBuffer[Edge](6000000)
  Source.fromFile("C:\\Users\\Rorick\\IdeaProjects\\algo-class\\src\\main\\resources\\SCC.txt").getLines().foreach {
      line: String =>
        val Array(u, v) = line.split(" ")
      g += ((u.toInt, v.toInt))
  }
  println(new Graph(g: _*).sccs.view.sorted(Ordering[Int].reverse).take(5).force)
  // read input (u,v) pairs
  // form a graph
//  val graph = new Graph
  // get graph SCCs from graph sort descending take 5
//  graph.
  // append with zeros if necessary
}

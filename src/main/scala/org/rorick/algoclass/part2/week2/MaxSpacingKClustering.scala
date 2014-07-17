package org.rorick.algoclass.part2.week2

import scala.collection.mutable
import scala.io.Source

class MaxSpacingKClustering(val K: Int, points: Set[Point], distances: Set[D]) {
  def maxSpacing: Int = 0
}

/**
 * =Question 1=
 * In this programming problem and the next you'll code up the clustering algorithm from lecture for computing a
 * max-spacing k-clustering. Download the text file
 * [[http://spark-public.s3.amazonaws.com/algo2/datasets/clustering1.txt here]]. This file describes a distance function
 * (equivalently, a complete graph with edge costs). It has the following format:
 * {{{
 * [number_of_nodes]
 * [edge 1 node 1] [edge 1 node 2] [edge 1 cost]
 * [edge 2 node 1] [edge 2 node 2] [edge 2 cost]
 * ...
 * }}}
 *
 * There is one edge (i,j) for each choice of 1&le;i&lt;j&le;n, where n is the number of nodes. For example, the third
 * line of the file is "1 3 5250", indicating that the distance between nodes 1 and 3 (equivalently, the cost of the
 * edge (1,3)) is 5250. You can assume that distances are positive, but you should NOT assume that they are distinct.
 *
 * Your task in this problem is to run the clustering algorithm from lecture on this data set, where the target number k
 * of clusters is set to 4. What is the maximum spacing of a 4-clustering?
 */
object MaxSpacingKClustering extends App {
  private val lines = Source.fromInputStream(getClass.getResourceAsStream("clustering1.txt")).getLines()
  private val N = lines.next().toInt
  private val distances = mutable.Set.empty[D]
  private val points = mutable.Set.empty[Point]
  lines.foreach { line =>
    val Array(p, q, d) = line split " " map (_.toInt)
    distances += D(p, q, d)
    points += (p, q)
  }
  // number of distances from one point to another without reverse
  private val numDistances = (N - 1) * N / 2
  assert(distances.size == numDistances, s"Expected num distances is $numDistances, but was ${distances.size}")

  println(new MaxSpacingKClustering(4, points.toSet, distances.toSet).maxSpacing)
}

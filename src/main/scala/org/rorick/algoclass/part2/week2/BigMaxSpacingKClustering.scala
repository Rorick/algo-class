package org.rorick.algoclass.part2.week2

import scala.collection.mutable

/**
 * =Question 2=
 *
 * In this question your task is again to run the clustering algorithm from lecture, but on a MUCH bigger graph. So big,
 * in fact, that the distances (i.e., edge costs) are only defined implicitly, rather than being provided as an
 * explicit list.
 *
 * The data set is [[http://spark-public.s3.amazonaws.com/algo2/datasets/clustering_big.txt here]]. The format is:
 * {{{
 * [# of nodes] [# of bits for each node's label]
 * [first bit of node 1] ... [last bit of node 1]
 * [first bit of node 2] ... [last bit of node 2]
 * ...
 * }}}
 * For example, the third line of the file "0 1 1 0 0 1 1 0 0 1 0 1 1 1 1 1 1 0 1 0 1 1 0 1" denotes the 24 bits
 * associated with node #2.
 *
 * The distance between two nodes u and v in this problem is defined as the Hamming distance--- the number of differing
 * bits --- between the two nodes' labels. For example, the Hamming distance between the 24-bit label of node #2 above
 * and the label "0 1 0 0 0 1 0 0 0 1 0 1 1 1 1 1 1 0 1 0 0 1 0 1" is 3 (since they differ in the 3rd, 7th, and 21st
 * bits).
 *
 * The question is: what is the largest value of k such that there is a k-clustering with spacing at least 3? That is,
 * how many clusters are needed to ensure that no pair of nodes with all but 2 bits in common get split into different
 * clusters?
 *
 * NOTE: The graph implicitly defined by the data file is so big that you probably can't write it out explicitly, let
 * alone sort the edges by cost. So you will have to be a little creative to complete this part of the question. For
 * example, is there some way you can identify the smallest distances without explicitly looking at every pair of nodes?
 */
object BigMaxSpacingKClustering extends App {
  val lines = io.Source.fromInputStream(getClass.getResourceAsStream("clustering_big.txt")).getLines()
  val Array(n, numBits) = lines next() split " " map (_.toInt)
  val points = mutable.Set[Point]
  lines foreach { line =>
    points += Integer parseInt (line replace (" ", ""), 2)
    // Note: point value is not unique
    // so we need to track each point with number.. Or not? Because distance is determined by value, hence value is a
    // kind of coordinates so equal points have the same coordinates and all distances to all other points will be the
    // same. But it affects number of clusters. Still, forums offer to use sets to ignore
  }

  // take point
  // generate points differing in l bits
  // remove points not in set
  // generate only lexicographically larger to avoid reverse edges
  // somehow need to track processed edges (queue?)

//  Integer.
}

package org.rorick.algoclass.part1.week5

import scala.util.parsing.combinator.JavaTokenParsers

/**
 * Main class for Dijkstra shortest paths programming assignment.
 */
object DijkstraShortestPath extends App with JavaTokenParsers {

  /**
   * line ::== node {edge}
   * edge ::== node "," length
   * node ::== wholeNumber
   * length ::== wholeNumber
   *
   * @param input line to parse
   * @return parsed data
   */
  def parseLine(input: String): (Node, List[Edge]) = {
    (parseAll(line, input): @unchecked) match {
      case Success(result, _) => result
    }
  }

  def line: Parser[(Node, List[(Node, Int)])] = node ~ rep(edge) ^^ { case n ~ es => (n, es)}

  def edge: Parser[(Node, Int)] = node ~ "," ~ length ^^ { case n ~ "," ~ l => (n, l)}

  def node: Parser[Node] = wholeNumber ^^ (_.toInt)

  def length: Parser[Int] = wholeNumber ^^ (_.toInt)

  val graph = new DijkstraGraph
  io.Source.fromInputStream(getClass.getResourceAsStream("dijkstraData.txt")).getLines().foreach {
    line =>
      val (n, edge) = parseLine(line)
      graph.addNode(n, edge)
  }

  val distancesFromOne = graph.shortestPathsDistances(1)

  println(List(7, 37, 59, 82, 99, 115, 133, 165, 188, 197) map (distancesFromOne(_)) mkString ",")
  // 2599,2610,2947,2052,2367,2399,2029,2442,2505,3068
}

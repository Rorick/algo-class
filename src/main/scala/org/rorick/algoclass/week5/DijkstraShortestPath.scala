package org.rorick.algoclass.week5

import util.parsing.combinator.JavaTokenParsers

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
    (parseAll(line, input): @unchecked) match {case Success(result, _) => result}
  }

  def line: Parser[(Node, List[Edge])] = node~rep(edge) ^^ {case (n: Node)~(es: List[Edge]) => (n, es)}
  def edge: Parser[(Node, Int)] = node~","~length ^^ {case n~","~l => (n, l)}
  def node: Parser[Node] = wholeNumber ^^ (_.toInt)
  def length: Parser[Int] = wholeNumber ^^ (_.toInt)

  io.Source.fromInputStream(getClass.getResourceAsStream("dijkstraData.txt")).getLines().foreach {
    line =>
      println(parseLine(line))
  }
}

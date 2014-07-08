package org.rorick.algoclass.part1.week1.inversions

import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
 * Programming assignment of first week.
 */
object Main extends App {
  var lines = ListBuffer[Int]()
  Source.fromFile("C:\\Users\\Rorick\\IdeaProjects\\algo-class\\src\\main\\resources\\IntegerArray.txt").getLines().foreach { line =>
    lines += line.toInt
  }

  val invs = inversions.inversions(lines.toList)
  println(invs)
}

package org.rorick.algoclass.week1.inversions

import io.Source
import collection.mutable.ListBuffer

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

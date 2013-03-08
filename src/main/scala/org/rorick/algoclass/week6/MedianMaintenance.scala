package org.rorick.algoclass.week6


/**
 * Solution for median maintenance problem for week 6.
 */
object MedianMaintenance extends App {
  val maintainer = new MedianMaintainer
  var sum = 0
  io.Source.fromInputStream(getClass.getResourceAsStream("Median.txt")).getLines().foreach {
    line =>
      assert(line.toInt > 0)
      maintainer.add(line.toInt)
      val median = maintainer.median
      sum += median
      assert(sum > 0)
  }

  println(sum %  10000)
}

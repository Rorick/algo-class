package org.rorick.algoclass.part1.week6

import java.util

import scala.collection.mutable.ListBuffer

/**
 * Main class for two sum problem of week 6.
 */
object TwoSum extends App {
  val nums = ListBuffer[Int]()
  io.Source.fromInputStream(getClass.getResourceAsStream("HashInt.txt")).getLines().foreach {
    line =>
      nums += line.toInt
  }

  val distinctNums = nums.sorted.distinct.toArray

  var count = 0
  2500 to 4000 foreach {
    t =>
      var notFound = true
      var i = 0
      while (notFound && i < distinctNums.length) {
        val x = distinctNums(i)
        val y = t - x
        if (x != y && util.Arrays.binarySearch(distinctNums, y) >= 0) {
          count += 1
          notFound = false
        }
        i += 1
      }
  }
  println(count)
}

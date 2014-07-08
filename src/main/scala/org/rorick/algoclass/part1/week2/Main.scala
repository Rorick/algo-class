package org.rorick.algoclass.part1.week2

import scala.io.Source

/**
 * Programming assignment for week2.
 */
object Main extends App {
  val a = new Array[Int](10000)
  var i = 0
  Source.fromFile("C:\\Users\\Rorick\\IdeaProjects\\algo-class\\src\\main\\resources\\QuickSort.txt").getLines().foreach {line =>
    a(i) = line.toInt
    i = i + 1
  }

  import org.rorick.algoclass.part1.week2.QuickSort._
  println(quicksortCounting(a.clone())(firstElement))
  println(quicksortCounting(a.clone())(lastElement))
  println(quicksortCounting(a.clone())(medianOfThree))

  def lastElement(a: Array[Int], i: Int, j: Int) = j

  def medianOfThree(a: Array[Int], i: Int, j: Int) = {
    // should write TDD next time
    val k = i + (j - i) / 2
    val (x, y, z) = (a(i), a(k), a(j))

    if ((y < x && x < z) || (z < x && x < y)) i
    else if ((x < y && y < z) || (z < y && y < x)) k
    else j
  }

  assert(medianOfThree(Array(4, 5, 6, 7), 0, 3) == 1)
  assert(medianOfThree(Array(8, 2, 4, 5, 7, 1), 0, 5) == 2)
  assert(quicksortCounting(Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))(medianOfThree) == 19)
  assert(quicksortCounting(Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).reverse)(medianOfThree) == 19)
  assert(quicksortCounting((1 to 100).toArray)(medianOfThree) == 480)
  assert(quicksortCounting((1 to 100).reverse.toArray)(medianOfThree) == 1302)
  assert(quicksortCounting(Array(2, 8, 9, 3, 7, 5, 10, 1, 6, 4))(medianOfThree) == 19)
}

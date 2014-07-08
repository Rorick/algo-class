package org.rorick.algoclass.part1.week3

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FunSuite, Matchers}

import scala.util.Random

/**
 * Test case for [[org.rorick.algoclass.part1.week3.MinCutFinder]].
 */
class MinCutFinderTest extends FunSuite with Matchers with PropertyChecks {

  test("Min cut of one vertex graph should be zero") {
    val result = new MinCutFinder(List()).minCutSize

    result should equal(0)
  }

  test("Min cut of connected two vertices graph should be one") {
    val result = new MinCutFinder(List(List(1, 2), List(2, 1))).minCutSize

    result should equal(1)
  }

  test("Min cut of connected three vertices graph should be two") {
    val result = new MinCutFinder(List(List(1, 2, 3), List(2, 1, 3), List(3, 1, 2))).minCutSize

    result should equal(2)
  }

  test("Min cut of three vertices tree graph should be one") {
    val result = new MinCutFinder(List(List(0, 1, 2), List(1, 0), List(2, 0))).minCutSize

    result should equal(1)
  }

  test("Min cut of four vertices connected graph should be three") {
    val result = new MinCutFinder(List(
      List(1, 2, 3, 4), List(2, 1, 3, 4), List(3, 1, 2, 4), List(4, 1, 2, 3))).minCutSize

    result should equal(3)
  }

  test("Min cut of four vertices graph should depend on random seed") {
    val graph1 = List(List(1, 2, 3), List(2, 1, 3, 4), List(3, 1, 2, 4), List(4, 2, 3))
    val data = Table(
      ("graph", "seed", "min cut size"),
      (graph1, 1361474496926L, 2),
      (graph1, 1361475021702L, 3)
    )

    forAll(data) {
      (graph: List[List[Int]], seed: Long, expectedSize: Int) =>
        val random = new Random(seed)
        val actualSize = new MinCutFinder(graph, n => random.nextInt(n)).minCutSize
        actualSize should (equal(expectedSize))
    }
  }
}

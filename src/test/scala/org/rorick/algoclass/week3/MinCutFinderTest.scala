package org.rorick.algoclass.week3

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

/**
 * Test case for [[org.rorick.algoclass.week3.MinCutFinder]].
 */
class MinCutFinderTest extends FunSuite with ShouldMatchers {

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

  test("Min cut of four vertices connected graph should be tree") {
    val result = new MinCutFinder(List(
      List(1, 2, 3, 4), List(2, 1, 3, 4), List(3, 1, 2, 4), List(4, 1, 2, 3))).minCutSize

    result should equal(3)
  }
}

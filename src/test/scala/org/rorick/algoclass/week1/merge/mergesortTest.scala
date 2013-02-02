package org.rorick.algoclass.week1.merge

import org.scalatest.FunSuite

/**
 * Test case for [[org.rorick.algoclass.week1.merge.mergesort]].
 */
class mergesortTest extends FunSuite {
  import mergesort._

  test("should return same list when zero element list") {
    assert(sort(List()) === List())
  }

  test("should return same list when one element list") {
    assert(sort(List(2)) === List(2))
  }

  test("should return same list when two element sorted list") {
    assert(sort(List(2, 5)) === List(2, 5))
  }

  test("should return same list when three element sorted list") {
    assert(sort(List(2, 5, 6)) === List(2, 5, 6))
  }

  test("should return sorted list when two element unsorted list") {
    assert(sort(List(5, 2)) === List(2, 5))
  }

  test("should return new list") {
    val unsortedList = List(5, 2)
    assert(!(sort(unsortedList) eq unsortedList))

    val sortedList = List(3, 4)
    assert(!(sort(sortedList) eq sortedList))
  }
}

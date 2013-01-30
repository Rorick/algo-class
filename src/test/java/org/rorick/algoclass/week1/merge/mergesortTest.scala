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

  test("should return sorted list when two element unsorted list") {
    assert(sort(List(5, 2)) === List(2, 5))
  }
}

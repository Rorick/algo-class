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

  test("should return sorted list when three element unsorted list") {
    assert(sort(List(5, 2, 4)) === List(2, 4, 5))
    assert(sort(List(5, 4, 2)) === List(2, 4, 5))
    assert(sort(List(4, 2, 5)) === List(2, 4, 5))
    assert(sort(List(4, 5, 2)) === List(2, 4, 5))
    assert(sort(List(2, 5, 4)) === List(2, 4, 5))
    assert(sort(List(2, 4, 5)) === List(2, 4, 5))
  }

  test("should return sorted list when four element list") {
    assert(sort(List(1, 2, 3, 4)) === List(1, 2, 3, 4))
    assert(sort(List(1, 2, 4, 3)) === List(1, 2, 3, 4))
    assert(sort(List(1, 3, 2, 4)) === List(1, 2, 3, 4))
    assert(sort(List(1, 3, 4, 2)) === List(1, 2, 3, 4))
    assert(sort(List(1, 4, 2, 3)) === List(1, 2, 3, 4))
    assert(sort(List(1, 4, 3, 2)) === List(1, 2, 3, 4))
    assert(sort(List(2, 1, 3, 4)) === List(1, 2, 3, 4))
    assert(sort(List(2, 1, 4, 3)) === List(1, 2, 3, 4))
    assert(sort(List(2, 3, 1, 4)) === List(1, 2, 3, 4))
    assert(sort(List(2, 3, 4, 1)) === List(1, 2, 3, 4))
    assert(sort(List(2, 4, 1, 3)) === List(1, 2, 3, 4))
    assert(sort(List(2, 4, 3, 1)) === List(1, 2, 3, 4))
    assert(sort(List(3, 1, 2, 4)) === List(1, 2, 3, 4))
    assert(sort(List(3, 1, 4, 2)) === List(1, 2, 3, 4))
    assert(sort(List(3, 2, 1, 4)) === List(1, 2, 3, 4))
    assert(sort(List(3, 2, 4, 1)) === List(1, 2, 3, 4))
    assert(sort(List(3, 4, 1, 2)) === List(1, 2, 3, 4))
    assert(sort(List(3, 4, 2, 1)) === List(1, 2, 3, 4))
    assert(sort(List(4, 1, 2, 3)) === List(1, 2, 3, 4))
    assert(sort(List(4, 1, 3, 2)) === List(1, 2, 3, 4))
    assert(sort(List(4, 2, 1, 3)) === List(1, 2, 3, 4))
    assert(sort(List(4, 2, 3, 1)) === List(1, 2, 3, 4))
    assert(sort(List(4, 3, 1, 2)) === List(1, 2, 3, 4))
    assert(sort(List(4, 3, 2, 1)) === List(1, 2, 3, 4))
  }

  test("should sort random 5 element lists") {
    val expected = (1 to 5).toList
    expected.permutations.foreach { xs =>
      assert(sort(xs) === expected, xs)
    }
  }

  test("should sort random 10 element lists") {
    val expected = (1 to 10).toList
    expected.permutations.foreach { xs =>
      assert(sort(xs) === expected, xs)
    }
  }

  test("should sort 5 element list") {
    assert(sort(List(3, 4, 5, 1, 2)) === List(1, 2, 3, 4, 5))
  }

  test("should return new list") {
    val unsortedList = List(5, 2)
    assert(!(sort(unsortedList) eq unsortedList))

    val sortedList = List(3, 4)
    assert(!(sort(sortedList) eq sortedList))
  }
}

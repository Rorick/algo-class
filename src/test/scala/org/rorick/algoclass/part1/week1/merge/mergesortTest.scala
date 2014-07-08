package org.rorick.algoclass.part1.week1.merge

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FunSuite, Matchers}

/**
 * Test case for [[org.rorick.algoclass.part1.week1.merge.mergesort]].
 */
class mergesortTest extends FunSuite with Matchers with PropertyChecks {

  import org.rorick.algoclass.part1.week1.merge.mergesort._

  test("should return same list when zero element list") {
    sort(List()) should equal(List())
  }

  test("should return same list when one element list") {
    sort(List(2)) should equal(List(2))
  }

  test("should return same list when two element sorted list") {
    sort(List(2, 5)) should equal(List(2, 5))
  }

  test("should return same list when three element sorted list") {
    sort(List(2, 5, 6)) should equal(List(2, 5, 6))
  }

  test("should return sorted list when two element unsorted list") {
    sort(List(5, 2)) should equal(List(2, 5))
  }

  test("should return sorted list when three element unsorted list") {
    val listsOf3 = Table(
      "list",
      List(5, 2, 4),
      List(5, 4, 2),
      List(4, 2, 5),
      List(4, 5, 2),
      List(2, 5, 4),
      List(2, 4, 5)
    )

    forAll(listsOf3) {
      xs: List[Int] =>
        sort(xs) should equal(List(2, 4, 5))
    }
  }

  test("should return sorted list when four element list") {
    val listsOf4 = Table(
      "list",
      List(1, 2, 3, 4),
      List(1, 2, 4, 3),
      List(1, 3, 2, 4),
      List(1, 3, 4, 2),
      List(1, 4, 2, 3),
      List(1, 4, 3, 2),
      List(2, 1, 3, 4),
      List(2, 1, 4, 3),
      List(2, 3, 1, 4),
      List(2, 3, 4, 1),
      List(2, 4, 1, 3),
      List(2, 4, 3, 1),
      List(3, 1, 2, 4),
      List(3, 1, 4, 2),
      List(3, 2, 1, 4),
      List(3, 2, 4, 1),
      List(3, 4, 1, 2),
      List(3, 4, 2, 1),
      List(4, 1, 2, 3),
      List(4, 1, 3, 2),
      List(4, 2, 1, 3),
      List(4, 2, 3, 1),
      List(4, 3, 1, 2),
      List(4, 3, 2, 1)
    )

    forAll (listsOf4) {
      xs: List[Int] =>
        sort(xs) should equal(List(1, 2, 3, 4))
    }
  }

  test("should sort random 5 element lists") {
    val expected = (1 to 5).toList
    expected.permutations.foreach { xs =>
      sort(xs) should equal (expected)
    }
  }

  test("should sort random 10 element lists") {
    val expected = (1 to 10).toList
    expected.permutations.foreach { xs =>
      sort(xs) should equal (expected)
    }
  }

  test("should sort random 100 element list") {
    val expected = (1 to 100).toList

    val shuffled = scala.util.Random.shuffle(expected)

    sort(shuffled) should equal (expected)
  }

  test("should sort 5 element list") {
    sort(List(3, 4, 5, 1, 2)) should equal(List(1, 2, 3, 4, 5))
  }

  test("should return new list") {
    val unsortedList = List(5, 2)
    sort(unsortedList) should (not be theSameInstanceAs(unsortedList))

    val sortedList = List(3, 4)
    sort(sortedList) should (not be theSameInstanceAs(sortedList))

    val emptyList = List()
    sort(emptyList) should be theSameInstanceAs emptyList
  }
}

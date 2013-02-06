package org.rorick.algoclass.week2

import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import org.scalatest.matchers.ShouldMatchers

/**
 * Test case for quick sort.
 */
class QuickSortTest extends FunSuite with PropertyChecks with ShouldMatchers {
  import QuickSort._

  test("partition should be not defined for empty array") {
    evaluating {
      partition(Array(), 0, 0)
    } should produce[IllegalArgumentException]
  }

  test("partition should be not defined for one element array") {
    evaluating {
      partition(Array(5), 0, 0)
    } should produce[IllegalArgumentException]
  }

  test("partition should correctly partition two or greater elements arrays") {
    val partitions = Table(
      ("a", "partitioned"),
      (Array(1, 2), Array(1, 2)),
      (Array(2, 1), Array(1, 2)),
      (Array(6, 3), Array(3, 6)),
      (Array(1, 2, 3), Array(1, 2, 3)),
      (Array(2, 1, 3), Array(1, 2, 3)),
      (Array(2, 3, 1), Array(1, 2, 3)),
      (Array(3, 1, 2), Array(2, 1, 3)),
      (Array(3, 2, 1), Array(1, 2, 3)),
      (Array(3, 1, 5, 4, 2), Array(2, 1, 3, 4, 5)),
      (Array(3, 8, 2, 5, 1, 4, 7, 6), Array(1, 2, 3, 5, 8, 4, 7, 6))
    )

    forAll(partitions) { (a: Array[Int], expected: Array[Int]) =>
      partition(a, 0, a.length - 1) should equal (expected)
    }
  }

  test("partition should correctly partition within the array") {
    val partitions = Table(
      ("a", "l", "h", "partitioned"),
      (Array(5, 6, 1, 2, 3, 4), 2, 3, Array(5, 6, 1, 2, 3, 4)),
      (Array(5, 6, 2, 1, 3, 4), 2, 3, Array(5, 6, 1, 2, 3, 4)),
      (Array(100, 100, 100, 3, 8, 2, 5, 1, 4, 7, 6, 100, 100, 100, 100), 3, 10, Array(100, 100, 100, 1, 2, 3, 5, 8, 4, 7, 6, 100, 100, 100, 100))
    )

    forAll(partitions) { (a: Array[Int], i: Int, j: Int, expected: Array[Int]) =>
      partition(a, i, j) should equal (expected)
    }
  }
}

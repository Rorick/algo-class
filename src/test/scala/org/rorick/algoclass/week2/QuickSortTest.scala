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
      partition(a, 0, a.length - 1)
      a should equal (expected)
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
      partition(a, i, j)
      a should equal (expected)
    }
  }

  test("partition should use pivot function") {
    val fifthElement = (i: Int, j: Int) => i + 3
    val a = Array(5, 8, 2, 3, 1, 4, 7, 6)
    val p = partition(a, 0, 7)(fifthElement)
    a should equal (Array(1, 2, 3, 5, 8, 4, 7, 6))
    p should equal (2)
  }

  test("quicksort should correctly sort array") {
    val arrays = Table[Array[Int], Array[Int]](
      ("a","expected"),
      (Array(), Array()),
      (Array(3), Array(3)),
      (Array(3, 5), Array(3, 5)),
      (Array(5, 3), Array(3, 5)),
      (Array(3, 1, 5), Array(1, 3, 5)),
      (Array(3, 5, 1), Array(1, 3, 5)),
      (Array(1, 3, 5), Array(1, 3, 5)),
      (Array(1, 5, 3), Array(1, 3, 5)),
      (Array(5, 3, 1), Array(1, 3, 5)),
      (Array(3, 2, 8, 5, 1, 4, 7, 6), Array(1, 2, 3, 4, 5, 6, 7, 8))
    )

    forAll(arrays) { (a, expected) =>
      val result = quicksort(a)
      result should equal (expected)
    }
  }
}

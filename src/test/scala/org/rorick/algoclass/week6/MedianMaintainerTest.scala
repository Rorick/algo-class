package org.rorick.algoclass.week6

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FunSuite, Matchers}

/**
 * Test case for [[org.rorick.algoclass.week6.MedianMaintainer]].
 */
class MedianMaintainerTest extends FunSuite with Matchers with PropertyChecks {
  test("should return median of elements") {
    val data = Table(
      ("elements", "median"),
      (List(3), 3),
      (List(2, 3), 2),
      (List(3, 2), 2),
      (List(1, 2, 3), 2),
      (List(1, 3, 2), 2),
      (List(2, 1, 3), 2),
      (List(2, 3, 1), 2),
      (List(3, 1, 2), 2),
      (List(3, 2, 1), 2),
      (List(7, 8, 1, 9, 2, 3, 4, 5, 6, 10), 5)
    )

    forAll(data) {
      (elements, median) =>
        val maintainer = new MedianMaintainer
        elements foreach maintainer.add
        maintainer.median should equal(median)
    }
  }
}

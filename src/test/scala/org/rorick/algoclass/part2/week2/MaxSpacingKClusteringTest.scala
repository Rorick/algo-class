package org.rorick.algoclass.part2.week2

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FunSuite, Matchers}

/**
 * Test case for [[MaxSpacingKClustering]] class.
 */
class MaxSpacingKClusteringTest extends FunSuite with Matchers with PropertyChecks {
  test("should calculate max spacing") {
    val data = Table(
      ("k", "points", "distances", "expected spacing"),
      (2, Set(1, 2), Set(D(1, 2, 4)), 4),
      (2, Set(1, 2, 3), Set(D(1, 2, 4), D(1, 3, 2), D(2, 3, 5)), 4),
      (3, (1 to 6).toSet, Set(
        D(1, 2, 8),
        D(1, 3, 15),
        D(1, 4, 15),
        D(1, 5, 15),
        D(1, 6, 15),
        D(2, 3, 1),
        D(2, 4, 12),
        D(2, 5, 5),
        D(2, 6, 6),
        D(3, 4, 16),
        D(3, 5, 5),
        D(3, 6, 3),
        D(4, 5, 7),
        D(4, 6, 16),
        D(5, 6, 2)
      ), 7)
    )

    forAll(data) { (K, points, distances, expectedSpacing) =>
      new MaxSpacingKClustering(K, points, distances).spacing should equal(expectedSpacing)
    }
  }
}

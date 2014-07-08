package org.rorick.algoclass.part1.week1.inversions

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FunSuite, Matchers}

/**
 * Test case for [[org.rorick.algoclass.part1.week1.inversions.inversions]].
 */
class inversionsTest extends FunSuite with PropertyChecks with Matchers {
  test("inversions of empty list") {
    inversions.inversions(List()) should equal (0)
  }

  test("inversions of one element list") {
    inversions.inversions(List(2)) should equal (0)
  }

  test("inversions of two element list") {
    inversions.inversions(List(1, 2)) should equal (0)

    inversions.inversions(List(2, 1)) should equal (1)
  }

  test("inversions of three element list") {
    val listsOf3inversions = Table(
      ("list", "inversions"),
      (List(1, 2, 3), 0),
      (List(1, 3, 2), 1),
      (List(2, 1, 3), 1),
      (List(2, 3, 1), 2),
      (List(3, 1, 2), 2),
      (List(3, 2, 1), 3)
    )

    forAll(listsOf3inversions) {(xs: List[Int], invs: Int) =>
      inversions.inversions(xs) should equal (invs)
    }
  }

  test("inversions of four element list") {
    val listsOf3inversions = Table(
      ("list", "inversions"),
      (List(1, 2, 3, 4), 0),
      (List(1, 2, 4, 3), 1),
      (List(1, 3, 2, 4), 1),
      (List(1, 3, 4, 2), 2),
      (List(1, 4, 2, 3), 2),
      (List(1, 4, 3, 2), 3),
      (List(2, 1, 3, 4), 1),
      (List(2, 1, 4, 3), 2),
      (List(2, 3, 1, 4), 2),
      (List(2, 3, 4, 1), 3),
      (List(2, 4, 1, 3), 3),
      (List(2, 4, 3, 1), 4),
      (List(3, 1, 2, 4), 2),
      (List(3, 1, 4, 2), 3),
      (List(3, 2, 1, 4), 3),
      (List(3, 2, 4, 1), 4),
      (List(3, 4, 1, 2), 4),
      (List(3, 4, 2, 1), 5),
      (List(4, 1, 2, 3), 3),
      (List(4, 1, 3, 2), 4),
      (List(4, 2, 1, 3), 4),
      (List(4, 2, 3, 1), 5),
      (List(4, 3, 1, 2), 5),
      (List(4, 3, 2, 1), 6)
    )

    forAll(listsOf3inversions) {(xs: List[Int], invs: Int) =>
      inversions.inversions(xs) should equal (invs)
    }
  }
}

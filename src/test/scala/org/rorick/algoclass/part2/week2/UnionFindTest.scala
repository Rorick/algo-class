package org.rorick.algoclass.part2.week2

import org.scalatest.{FunSuite, Matchers}

/**
 * Test case for [[UnionFind]] class.
 */
class UnionFindTest extends FunSuite with Matchers {
  test("should place points to separate clusters when initialized") {
    val unionFind = UnionFind(1, 2, 3)
    List(1, 2, 3) map (p => unionFind.find(p) should equal(p))
  }

  test("should unite clusters") {
    val unionFind = UnionFind(1, 2, 3)
    unionFind.union(1, 2)

    unionFind.find(1) should equal(1)
    unionFind.find(2) should equal(1)
    unionFind.find(3) should equal(3)
  }

  test("should fail when in the same cluster") {
    val unionFind = UnionFind(1, 2, 3)
    unionFind.union(1, 2)
    an [IllegalArgumentException] should be thrownBy {
      unionFind.union(1, 2)
    }

    unionFind.find(1) should equal(1)
    unionFind.find(2) should equal(1)
    unionFind.find(3) should equal(3)
  }

  test("should prefer larger clusters when unite") {
    val unionFind = UnionFind(1, 2, 3)
    unionFind.union(1, 2)
    unionFind.union(3, 1)

    unionFind.find(1) should equal(1)
    unionFind.find(2) should equal(1)
    unionFind.find(3) should equal(1)
  }

  test("should return current number of clusters") {
    val unionFind = UnionFind(1, 2, 3)
    unionFind.size should equal(3)
    unionFind.union(1, 2)
    unionFind.size should equal(2)
    unionFind.union(3, 1)
    unionFind.size should equal(1)
  }
}

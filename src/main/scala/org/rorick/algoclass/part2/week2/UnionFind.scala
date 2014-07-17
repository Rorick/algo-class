package org.rorick.algoclass.part2.week2

import scala.collection.mutable

/**
 * Union-Find data structure for maintaining clusters.
 */
class UnionFind(points: Seq[Point]) {
  private val leaders = new Array[Point](points.size + 1)
  private val members = new Array[mutable.ListBuffer[Point]](points.size + 1)
  private var curSize = points.size

  points.foreach { p =>
    leaders(p) = p
    members(p) = mutable.ListBuffer(p)
  }

  def union(p: Point, q: Point): Point = {
    val (leaderP, leaderQ) = (find(p), find(q))
    require(leaderP != leaderQ, "Should belong to different clusters")
    val (smaller, larger) = if (size(leaderP) >= size(leaderQ)) (leaderQ, leaderP) else (leaderP, leaderQ)
    updateMembership(smaller, larger)
    curSize -= 1
    leaders(larger)
  }

  private def size(p: Point) = members(p).size

  private def updateMembership(smaller: Point, larger: Point) {
    members(smaller).foreach { m =>
      leaders(m) = leaders(larger)
    }
    members(larger) ++= members(smaller)
    members(smaller).clear()
  }

  def find(p: Point): Point = leaders(p)

  def size: Int = curSize
}

object UnionFind {
  def apply(points: Point*) = new UnionFind(points)
}
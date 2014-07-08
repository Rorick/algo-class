package org.rorick.algoclass.part1.week6

import scala.collection.mutable

/**
 * Maintains median among given values. In this implementation we keep median at the head of `low` priority queue. If
 * `low` becomes longer that `high` + 1 we take element to high. If `high` becomes longer we take element to `low`. Thus,
 * we always keep them balanced, having equal size for even size, or low has one element more for odd size.
 */
class MedianMaintainer {
  val low = new mutable.PriorityQueue()(Ordering.Int)
  val high = new mutable.PriorityQueue()(Ordering.Int.reverse)

  def add(i: Int) {
    if (low.isEmpty || low.head >= i) {
      low.enqueue(i)
    } else {
      high.enqueue(i)
    }

    if (low.size - high.size > 1) {
      high.enqueue(low.dequeue())
    } else if (high.size - low.size > 0) {
      low.enqueue(high.dequeue())
    }
  }

  def median: Int = {
    low.head
  }

}

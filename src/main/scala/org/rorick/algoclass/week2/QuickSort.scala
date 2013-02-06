package org.rorick.algoclass.week2

/**
 * Quicksort implementation.
 */
object QuickSort {

  implicit def firstElement(i: Int, j: Int) = i

  def swap(a: Array[Int], i: Int, j: Int) {
    val t = a(i)
    a(i) = a(j)
    a(j) = t
  }

  private[week2] def partition(a: Array[Int], l: Int, h: Int)(implicit pivot: (Int, Int) => Int): Array[Int] = {
    require(a.length > 1)

    // put pivot on the first element
    swap(a, l, pivot(l, h))

    var i = l + 1
    (l + 1 to h) foreach { j =>
      if (a(l) > a(j)) {
        swap(a, i, j)
        i = i + 1
      }
    }
    swap(a, l, i - 1)
    a
  }
}

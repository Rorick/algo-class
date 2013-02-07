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

  private[week2] def partition(a: Array[Int], l: Int, h: Int)(implicit pivot: (Int, Int) => Int): Int = {
    require(a.length > 1)

    // put pivot on the first element
    val p = pivot(l, h)
    swap(a, l, p)

    var i = l + 1
    (l + 1 to h) foreach { j =>
      if (a(l) > a(j)) {
        swap(a, i, j)
        i = i + 1
      }
    }
    swap(a, l, i - 1)
    i - 1
  }

  def quicksort(a: Array[Int])(implicit pivot: (Int, Int) => Int): Array[Int] = {
    def quicksortImpl(a: Array[Int], l: Int, h: Int) {
      if (h - l + 1 > 1) {
        val pivotPos = partition(a, l, h)(pivot)
        quicksortImpl(a, l, pivotPos - 1)
        quicksortImpl(a, pivotPos + 1, h)
      }
    }

    quicksortImpl(a, 0, a.length - 1)
    a
  }
}

package org.rorick.algoclass.week1.merge

/**
 * Implementation of merge sort.
 */
object mergesort {

  def merge(xs: Array[Int], from: Int, m: Int, until: Int) {
    require(from <= m && m <= until)

    var (i, j) = (from, m)
    while (i < j && j < until) {
      if (xs(i) < xs(j)) {
        i = i + 1
      } else {
        val t = xs(i)
        xs(i) = xs(j)
        xs(j) = t
        j = j + 1
      }
    }
  }

  private def sortImpl(xs: Array[Int], from: Int, until: Int) {
    require(from <= until)

    val N: Int = until - from
    assert(N >= 0)

    if (N > 1) {
      val m = (from + until) >> 1
      sortImpl(xs, from, m)
      sortImpl(xs, m, until)
      merge(xs, from, m, until)
    }
  }


  def sort(xs: List[Int]): List[Int] = {
    val xsArray = xs.toArray
    sortImpl(xsArray, 0, xs.size)
    List(xsArray: _*)
  }
}

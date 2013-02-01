package org.rorick.algoclass.week1.merge

/**
 * Implementation of merge sort.
 */
object mergesort {

  def merge(xs: Array[Int], from: Int, m: Int, until: Int) {
    if (xs(from) > xs(m)) {
      val t = xs(from)
      xs(from) = xs(m)
      xs(m) = t
    }
  }

  private def sortImpl(xs: Array[Int], from: Int, until: Int) {
    require(from <= until)

    val N: Int = until - from
    assert(N >= 0)

    if (N > 1) {
      val n = N / 2
      sortImpl(xs, 0, n)
      merge(xs, 0, n, N)
    }
  }


  def sort(xs: List[Int]): List[Int] = {
    val xsArray = xs.toArray
    sortImpl(xsArray, 0, xs.size)
    List(xsArray: _*)
  }
}

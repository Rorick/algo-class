package org.rorick.algoclass.week1.merge

/**
 * Implementation of merge sort.
 */
object mergesort {

  def merge(xs: Array[Int], ys: Array[Int], xfrom: Int, xuntil: Int, yfrom: Int, yuntil: Int): Array[Int] = {
    if (xs(xfrom) < ys(yfrom)) Array(xs(xfrom), ys(yfrom)) else Array(ys(yfrom), xs(xfrom))
  }

  private def sortImpl(xs: Array[Int], from: Int, until: Int): Array[Int] = {
    require(from <= until)

    val N: Int = until - from
    assert(N >= 0)

    if (N <= 1) {
      xs
    } else {
      val n = N / 2
      merge(sortImpl(xs, 0, n), sortImpl(xs, n, N), 0, n, n, N)
    }
  }


  def sort(xs: List[Int]): List[Int] = {
    List(sortImpl(xs.toArray, 0, xs.size): _*)
  }
}

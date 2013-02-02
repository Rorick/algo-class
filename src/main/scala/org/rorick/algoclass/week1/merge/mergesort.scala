package org.rorick.algoclass.week1.merge

/**
 * Implementation of merge sort.
 */
object mergesort {

  def merge(xs: Array[Int], from: Int, m: Int, to: Int) {
    require(from <= m && m <= to)

    var (i, j) = (from, m)
    while (i < j && j <= to) {
      while (xs(i) < xs(j) && i < j) {
        i = i + 1
      }
      val k = i
      while (xs(i) >= xs(j) && i < j) {
        val t = xs(i)
        xs(i) = xs(j)
        xs(j) = t
        i = i + 1
      }

      // we need to move to next j, but i should be the next after starting because we only can be sure that array is
      // sorted at `from` to `k`
      i = k + 1
      j = j + 1
    }
  }

  private def sortImpl(xs: Array[Int], from: Int, to: Int) {
    val N: Int = to - from + 1
    assert(N >= 0)

    if (N > 1) {
      val m = (from + to) >> 1
      sortImpl(xs, from, m)
      sortImpl(xs, m + 1, to)
      merge(xs, from, m + 1, to)
    }
  }


  def sort(xs: List[Int]): List[Int] = {
    val xsArray = xs.toArray
    sortImpl(xsArray, 0, xs.size - 1)
    List(xsArray: _*)
  }
}

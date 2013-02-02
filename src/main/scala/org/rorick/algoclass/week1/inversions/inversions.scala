package org.rorick.algoclass.week1.inversions

/**
 * Calculate number of inversions in array via mergesort.
 */
object inversions {
  def merge(xs: Array[Int], from: Int, m: Int, to: Int): Int = {
    require(from <= m && m <= to)

    var (i, j) = (from, m)
    var splitInversions = 0
    while (i < j && j <= to) {
      while (xs(i) < xs(j) && i < j) {
        i = i + 1
      }
      val k = i
      while (xs(i) >= xs(j) && i < j) {
        val t = xs(i)
        xs(i) = xs(j)
        xs(j) = t
        splitInversions = splitInversions + 1
        i = i + 1
      }

      // we need to move to next j, but i should be the next after starting because we only can be sure that array is
      // sorted at `from` to `k`
      i = k + 1
      j = j + 1
    }
    splitInversions
  }

  private def sortImpl(xs: Array[Int], from: Int, to: Int): Int = {
    val N: Int = to - from + 1
    assert(N >= 0)

    if (N > 1) {
      val m = (from + to) >> 1
      val leftInversions = sortImpl(xs, from, m)
      val rightInversions = sortImpl(xs, m + 1, to)
      val splitInversion = merge(xs, from, m + 1, to)
      leftInversions + rightInversions + splitInversion
    } else {
      0
    }
  }


  def inversions(xs: List[Int]): Int = {
    val xsArray = xs.toArray
    sortImpl(xsArray, 0, xs.size - 1)
  }
}

package org.rorick.algoclass.week1.merge

/**
 * Implementation of merge sort.
 */
object mergesort {

  def merge(xs: Array[Int], from: Int, m: Int, until: Int): Array[Int] = {
    if (xs(from) > xs(m)) {
      val t = xs(from)
      xs(from) = xs(m)
      xs(m) = t
    }
    xs
  }

  private def sortImpl(xs: Array[Int], from: Int, until: Int): Array[Int] = {
    require(from <= until)

    val N: Int = until - from
    assert(N >= 0)

    if (N <= 1) {
      xs
    } else {
      val n = N / 2
      val partiallySorted = sortImpl(xs, 0, n)
      merge(partiallySorted, 0, n, N)
    }
  }


  def sort(xs: List[Int]): List[Int] = {
    List(sortImpl(xs.toArray, 0, xs.size): _*)
  }
}

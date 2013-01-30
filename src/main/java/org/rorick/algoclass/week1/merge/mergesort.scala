package org.rorick.algoclass.week1.merge

/**
 * Implementation of merge sort.
 */
object mergesort {

  def merge(xs: List[Int], ys: List[Int]): List[Int] = {
    if(xs(0) < ys(0)) List(xs(0), ys(0)) else List(ys(0), xs(0))
  }

  def sort(xs: List[Int]): List[Int] = {
    val N = xs.size
    if (N <= 1) {
      xs
    } else {
      val n = N / 2
      merge(sort(xs.slice(0, n)), sort(xs.slice(n, N)))
    }
  }
}

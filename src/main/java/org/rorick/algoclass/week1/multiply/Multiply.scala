package org.rorick.algoclass.week1.multiply

/**
 * Multiply with presumably less than quadratic time.
 */
object Multiply extends App {
  def multiply(x: List[Int], y: List[Int]): Int = {
    if (x.size == y.size) {
      multiplyEq(x, y)
    } else {
      val (x_longer, y_shorter) = if (x.size > y.size) (x, y) else (y, x)
      10 * multiply(x_longer.slice(0, x_longer.size - 1), y_shorter) + multiply(x_longer.slice(x_longer.size - 1, x_longer.size), y_shorter)
    }
  }

  def multiplyEq(x: List[Int], y: List[Int]): Int = {
    require(x.size == y.size)
    val N = x.size
    if (N == 1) {
      x(0) * y(0)
    } else {
      val n = N / 2
      val a = x.slice(0, n)
      val b = x.slice(n, N)
      val c = y.slice(0, n)
      val d = y.slice(n, N)

      // (a*10^n + b)*(c*10^n + d) = a*c*10^N + (a*d + b*c)*10^n + b*d
      // a*d + b*c + a*c + b*d = (a + b)*d + (a+ b)*c = (a + b)*(c + d)
      val ac = multiply(a, c)
      val bd = multiply(b, d)
      val ad_plus_bc = multiply(plus(a, b), plus(c, d)) - ac - bd
      (ac * math.pow(10, N) + ad_plus_bc * math.pow(10, n) + bd).toInt
    }
  }

  def plus(x: List[Int], y: List[Int]): List[Int] = {
    def plusImpl(xy: List[(Int, Int)], t: Int): List[Int] = {
      xy match {
        case (a, b) :: rest =>
          val s = a + b + t
          s % 10 :: plusImpl(rest, s / 10)
        case rest => if (t > 0) t :: List.empty else List.empty
      }
    }
    val N = math.max(x.size, y.size)
    val x1 = if (x.size < N) 0 :: x else x
    val y1 = if (y.size < N) 0 :: y else y
    val xy = x1 zip y1
    plusImpl(xy.reverse, 0).reverse
  }

  assert(multiply(List(2, 0), List(2, 0)) == 400)
  assert(multiply(List(2, 0), List(2, 5)) == 500)
  assert(multiply(List(2, 5), List(2, 0)) == 500)
  assert(multiply(List(5, 6), List(1, 2)) == 672)
  assert(multiply(List(7, 8), List(3, 4)) == 2652)
  assert(multiply(List(1, 1), List(3)) == 33)
  assert(multiply(List(1, 5), List(7)) == 105)
  assert(multiply(List(1, 3, 4), List(4, 6)) == 6164)
  assert(multiply(List(5, 6, 7, 8), List(1, 2, 3, 4)) == 7006652)
}

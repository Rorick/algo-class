package org.rorick.algoclass.part2

import scala.collection.mutable

/**
 * Common stuff for schedulers etc.
 */
package object week1 {

  case class Job(w: Int, l: Int)(implicit val rankFunction: (Double, Double) => Double) {
    val rank: Double = rankFunction(w, l)
  }

  def weightedSum(schedule: Seq[Job]): Long =
    schedule.foldLeft((0, 0L)) { (agg, job) =>
      val (completionTime, sum) = agg
      val newCompletionTime = completionTime + job.l
      (newCompletionTime, sum + job.w * newCompletionTime)
    }._2

  class JobOrdering extends Ordering[Job] {
    override def compare(x: Job, y: Job): Int = {
      if (x.rank < y.rank) {
        -1
      } else if (x.rank > y.rank) {
        1
      } else {
        x.w - y.w
      }
    }
  }

  implicit val ordering = new Ordering[Job] {
    override def compare(x: Job, y: Job): Int = {
      if (x.rank < y.rank) {
        -1
      } else if (x.rank > y.rank) {
        1
      } else {
        x.w - y.w
      }
    }
  }

  type Node = Int

  case class Edge(u: Int, v: Int, l: Int) {
    require(u < v, "Edge must have u < v")

    def crossesCut(A: mutable.Set[Node], B: mutable.Set[Node]): Boolean = {
      A(u) && B(v) || A(v) && B(u)
    }
  }
}

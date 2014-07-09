package org.rorick.algoclass.part2.week1

import scala.collection.mutable


/**
 * In this programming problem and the next you'll code up the greedy algorithms from lecture for minimizing the
 * weighted sum of completion times.. Download the text file
 * [[http://spark-public.s3.amazonaws.com/algo2/datasets/jobs.txt here]]. This file describes a set of jobs with
 * positive and integral weights and lengths. It has the format
 * {{{
 * [number_of_jobs]
 * [job_1_weight] [job_1_length]
 * [job_2_weight] [job_2_length]
 * ...
 * }}}
 * For example, the third line of the file is "74 59", indicating that the second job has weight 74 and length 59. You
 * should NOT assume that edge weights or lengths are distinct.
 *
 * Your task in this problem is to run the greedy algorithm that schedules jobs in decreasing order of the difference
 * (weight - length). Recall from lecture that this algorithm is not always optimal. IMPORTANT: if two jobs have equal
 * difference (weight - length), you should schedule the job with higher weight first. Beware: if you break ties in a
 * different way, you are likely to get the wrong answer. You should report the sum of weighted completion times of the
 * resulting schedule --- a positive integer --- in the box below.
 *
 * ADVICE: If you get the wrong answer, try out some small test cases to debug your algorithm (and post your test cases to the discussion forum)!
 */
object GreedySchedulerStraightForward extends App {
  def scheduleJobs(jobs: Seq[Job]): Seq[Job] = {
    val queue = mutable.PriorityQueue[Job](jobs: _*)(new Ordering[Job] {
      override def compare(x: Job, y: Job): Int = {
        if (x.rank < y.rank) {
          -1
        } else if (x.rank > y.rank) {
          1
        } else {
          x.w - y.w
        }
      }
    })

    queue.dequeueAll.toList
  }

  def weightedSum(schedule: Seq[Job]): Long =
    schedule.foldLeft((0, 0L)) { (agg, job) =>
      val (completionTime, sum) = agg
      val newCompletionTime = completionTime + job.l
      (newCompletionTime, sum + job.w * newCompletionTime)
    }._2

  private val lines = io.Source.fromInputStream(getClass.getResourceAsStream("jobs.txt")).getLines()
  private val numJobs: Int = lines.next().toInt

  implicit def rankFunction(w: Int, l: Int) = w - l

  case class Job(w: Int, l: Int)(implicit val rankFunction: (Int, Int) => Int) {
    val rank = rankFunction(w, l)
  }

  val jobs = new mutable.ArrayBuffer[Job](numJobs)
  lines.foreach { line =>
    val Array(w, l) = line.split(" ")
    jobs += Job(w.toInt, l.toInt)
  }
  assert(jobs.size == numJobs, "Incorrect number of jobs in input file")

  val schedule: Seq[Job] = scheduleJobs(jobs)

  println(weightedSum(jobs))
  println(weightedSum(schedule))
}
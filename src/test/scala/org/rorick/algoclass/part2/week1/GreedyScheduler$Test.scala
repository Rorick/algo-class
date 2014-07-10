package org.rorick.algoclass.part2.week1

import org.rorick.algoclass.part2.week1.GreedyScheduler.scheduleJobs
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FunSuite, Matchers}

/**
 * Test case for [[GreedyScheduler]] object.
 */
class GreedyScheduler$Test extends FunSuite with Matchers with PropertyChecks {
  implicit def rankFunction(w: Double, l: Double) = w - l

  val randomCorrectRankJobs: Gen[List[Job]] = Gen.nonEmptyListOf(for {
    w <- Gen.choose(1, 100)
    l <- Gen.choose(1, 100)
  } yield Job(w, l)(_ / _))

  test("should schedule empty jobs") {
    scheduleJobs(List.empty[Job]) should be(empty)
  }

  test("should schedule jobs") {
    val data = Table(
      ("jobs", "schedule"),
      (List(Job(4, 5)), List(Job(4, 5))),
      (List(Job(3, 4), Job(4, 4)), List(Job(4, 4), Job(3, 4))),
      (List(Job(4, 4), Job(3, 4)), List(Job(4, 4), Job(3, 4))),
      (List(Job(2, 4), Job(1, 2), Job(4, 4)), List(Job(4, 4), Job(1, 2), Job(2, 4))),
      (List(Job(2, 4), Job(4, 4), Job(1, 2)), List(Job(4, 4), Job(1, 2), Job(2, 4))),
      (List(Job(1, 2), Job(2, 4), Job(4, 4)), List(Job(4, 4), Job(1, 2), Job(2, 4))),
      (List(Job(1, 2), Job(4, 4), Job(2, 4)), List(Job(4, 4), Job(1, 2), Job(2, 4))),
      (List(Job(4, 4), Job(2, 4), Job(1, 2)), List(Job(4, 4), Job(1, 2), Job(2, 4))),
      (List(Job(4, 4), Job(1, 2), Job(2, 4)), List(Job(4, 4), Job(1, 2), Job(2, 4)))
    )

    forAll(data) { (jobs, schedule) =>
      scheduleJobs(jobs) should be(schedule)
    }
  }

  test("should schedule jobs with correct rank function") {
    implicit def rankFunction(w: Double, l: Double) = w / l

    val data = Table(
      ("jobs", "schedule"),
      (List(Job(4, 5)), List(Job(4, 5))),
      (List(Job(3, 4), Job(4, 4)), List(Job(4, 4), Job(3, 4))),
      (List(Job(4, 4), Job(3, 4)), List(Job(4, 4), Job(3, 4))),
      (List(Job(2, 4), Job(1, 2), Job(4, 4)), List(Job(4, 4), Job(2, 4), Job(1, 2))),
      (List(Job(2, 4), Job(4, 4), Job(1, 2)), List(Job(4, 4), Job(2, 4), Job(1, 2))),
      (List(Job(1, 2), Job(2, 4), Job(4, 4)), List(Job(4, 4), Job(2, 4), Job(1, 2))),
      (List(Job(1, 2), Job(4, 4), Job(2, 4)), List(Job(4, 4), Job(2, 4), Job(1, 2))),
      (List(Job(4, 4), Job(2, 4), Job(1, 2)), List(Job(4, 4), Job(2, 4), Job(1, 2))),
      (List(Job(4, 4), Job(1, 2), Job(2, 4)), List(Job(4, 4), Job(2, 4), Job(1, 2)))
    )

    forAll(data) { (jobs, schedule) =>
      scheduleJobs(jobs) should be(schedule)
    }
  }

  // This test is non-deterministic and may fail due to possibility of having different schedules of the same jobs
  // with same weighted sum
  test("should schedule jobs with correct rank function to achieve minimum weighted sum") {
    def bruteForceSchedule(jobs: List[Job]) = {
      jobs.permutations.map(jobs => (weightedSum(jobs), jobs)).toList.sortBy(_._1).head._2
    }

    // limit size of list due to exponential growth of permutations number and possible OutOfMemoryError
    forAll(randomCorrectRankJobs, maxSize(7)) { jobs =>
      scheduleJobs(jobs) should be(bruteForceSchedule(jobs))
    }
  }

  test("should prefer jobs with higher weight when has ties") {
    scheduleJobs(List(Job(4, 4), Job(5, 5))) should be(List(Job(5, 5), Job(4, 4)))
  }

  test("should not miss jobs after schedule") {
    forAll(randomCorrectRankJobs) { jobs =>
      scheduleJobs(jobs).size should be(jobs.size)
    }
  }

  test("should correctly calculate weighted sum") {
    val data = Table(
      ("schedule", "weightedSum"),
      (List.empty[Job], 0),
      (List(Job(3, 4)), 3 * 4),
      (List(Job(3, 4), Job(4, 4)), 3 * 4 + 4 * (4 + 4)),
      (List(Job(4, 4), Job(3, 4)), 4 * 4 + 3 * (4 + 4)),
      (List(Job(4, 4), Job(1, 2), Job(2, 4)), 4 * 4 + 1 * (4 + 2) + 2 * (4 + 2 + 4)),
      (List(Job(4, 4), Job(2, 4), Job(1, 2)), 4 * 4 + 2 * (4 + 4) + 1 * (4 + 4 + 2))
    )

    forAll(data) { (schedule, expectedWeightedSum) =>
      weightedSum(schedule) should be(expectedWeightedSum)
    }
  }
}

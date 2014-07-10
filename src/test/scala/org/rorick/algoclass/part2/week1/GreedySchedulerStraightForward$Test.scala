package org.rorick.algoclass.part2.week1

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FunSuite, Matchers}

/**
 * Test case for [[GreedySchedulerStraightForward]] object.
 */
class GreedySchedulerStraightForward$Test extends FunSuite with Matchers with PropertyChecks {

  import org.rorick.algoclass.part2.week1.GreedySchedulerStraightForward.scheduleJobs

  implicit def rankFunction(w: Double, l: Double) = w - l

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

  test("should schedule jobs with correct rank function to achieve minimum weighted sum")(pendingUntilFixed {
    import org.scalacheck.Gen
    implicit def rankFunction(w: Double, l: Double) = w / l

    val job: Gen[Job] = for {
      w <- Gen.choose(1, 100)
      l <- Gen.choose(1, 100)
    } yield Job(w, l)

    val jobs: Gen[List[Job]] = Gen.listOf(job) //suchThat (_.nonEmpty)

    def bruteForceSchedule(jobs: List[Job]) =
      jobs.permutations.map(jobs => (weightedSum(jobs), jobs)).toList.sortBy(_._1).head._2


    forAll(jobs, minSuccessful(10)) { jobs =>
      scheduleJobs(jobs) should be(bruteForceSchedule(jobs))
    }
  })

  test("should prefer jobs with higher weight when has ties") {
    scheduleJobs(List(Job(4, 4), Job(5, 5))) should be(List(Job(5, 5), Job(4, 4)))
  }

  test("should not miss jobs after schedule")(pending)

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

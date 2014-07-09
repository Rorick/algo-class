package org.rorick.algoclass.part2.week1

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FunSuite, Matchers}

/**
 * Test case for [[GreedySchedulerStraightForward]] object.
 */
class GreedySchedulerStraightForward$Test extends FunSuite with Matchers with PropertyChecks {

  import org.rorick.algoclass.part2.week1.GreedySchedulerStraightForward.{Job, rankFunction, scheduleJobs, weightedSum}

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
      (List(Job(4, 4), Job(3, 4)), 4 * 4 + 3 * (4 + 4))
    )

    forAll(data) {(schedule, expectedWeightedSum) =>
      weightedSum(schedule) should be(expectedWeightedSum)
    }
  }
}

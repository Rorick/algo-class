package org.rorick.algoclass.week3

import org.scalatest.FunSuite
import org.scalatest.matchers.{MatchResult, Matcher, ShouldMatchers}

/**
 * Test case for [[org.rorick.algoclass.week3.Graph]].
 */
class GraphTest extends FunSuite with ShouldMatchers {

  import Graph.Edge
  import GraphTest._

  test("One node graph") {
    val graph = new Graph(List(List(1)))
    graph.nodes should (contain[Node](SimpleNode(1)) and have length (1))
    graph.edges should be('empty)
    graph.incidentals(SimpleNode(1)) should be('empty)
  }

  test("Two nodes disjoint graph") {
    val graph = new Graph(List(List(1), List(2)))
    graph.nodes should (containAll[Node](SimpleNode(1), SimpleNode(2)) and have length (2))
    graph.edges should be('empty)
    graph.incidentals(SimpleNode(1)) should be('empty)
    graph.incidentals(SimpleNode(2)) should be('empty)
  }

  test("Two nodes connected graph") {
    val graph = new Graph(List(List(1, 2), List(2, 1)))
    graph.nodes should (containAll[Node](SimpleNode(1), SimpleNode(2)) and have length (2))
    graph.edges should (
      containAll[Edge]((SimpleNode(1), SimpleNode(2)), (SimpleNode(2), SimpleNode(1))) and
        have length (2))
    graph.incidentals(SimpleNode(1)) should (containAll[Edge]((SimpleNode(1), SimpleNode(2)), (SimpleNode(2), SimpleNode(1))) and have length (2))
    graph.incidentals(SimpleNode(2)) should (containAll[Edge]((SimpleNode(1), SimpleNode(2)), (SimpleNode(2), SimpleNode(1))) and have length (2))
  }

  test("Three nodes linear graph") {
    val graph = new Graph(List(List(1, 2), List(2, 3), List(3)))
    graph.nodes should (containAll[Node](SimpleNode(1), SimpleNode(2), SimpleNode(3)) and have length (3))
    graph.edges should (
      containAll[Edge]((SimpleNode(1), SimpleNode(2)), (SimpleNode(2), SimpleNode(3))) and
        have length (2))
    graph.incidentals(SimpleNode(1)) should (contain[Edge](SimpleNode(1), SimpleNode(2)) and have length (1))
    graph.incidentals(SimpleNode(2)) should (containAll[Edge]((SimpleNode(1), SimpleNode(2)), (SimpleNode(2), SimpleNode(3))) and have length (2))
    graph.incidentals(SimpleNode(3)) should (contain[Edge](SimpleNode(2), SimpleNode(3)) and have length(1))
  }

  test("Three nodes linear undirected graph") {
    val graph = new Graph(List(List(1, 2), List(2, 1, 3), List(3, 2)))
    graph.nodes should (containAll[Node](SimpleNode(1), SimpleNode(2), SimpleNode(3)) and have length (3))
    graph.edges should (
      containAll[Edge](
        (SimpleNode(1), SimpleNode(2)),
        (SimpleNode(2), SimpleNode(1)),
        (SimpleNode(2), SimpleNode(3)),
        (SimpleNode(3), SimpleNode(2)))
        and have length (4))
    graph.incidentals(SimpleNode(1)) should (containAll[Edge]((SimpleNode(1), SimpleNode(2)), (SimpleNode(2), SimpleNode(1))) and have length (2))
    graph.incidentals(SimpleNode(2)) should (
      containAll[Edge]((SimpleNode(2), SimpleNode(3)), (SimpleNode(3), SimpleNode(2)), (SimpleNode(2), SimpleNode(1)), (SimpleNode(1), SimpleNode(2)))
      and have length (4))
    graph.incidentals(SimpleNode(3)) should (containAll[Edge]((SimpleNode(3), SimpleNode(2)), (SimpleNode(2), SimpleNode(3))) and have length(2))
  }

  test("Contract edges") {
    val graph = new Graph(List(List(1, 2, 3, 4), List(2, 1, 3, 4), List(3, 1, 2, 4), List(4, 1, 2, 3)))
    graph.contractNode(0)

    // contracted and a loop
    graph.edges should not (contain[Edge] (SimpleNode(1), SimpleNode(2)) and contain[Edge] (SimpleNode(2), SimpleNode(1)))
    val merged = new MergedNode(SimpleNode(1), SimpleNode(2))
    graph.edges should (containAll[Edge] ((merged, SimpleNode(3)), (merged, SimpleNode(4)), (SimpleNode(3), merged), (SimpleNode(4), merged)))

    graph.incidentals(SimpleNode(1)) should be ('empty)
    graph.incidentals(SimpleNode(2)) should be ('empty)
    graph.incidentals(merged) should (contain[Edge](merged, SimpleNode(3)) and contain[Edge](merged, SimpleNode(4)))
    graph.incidentals(SimpleNode(3)) should (contain[Edge](SimpleNode(3), merged))
    graph.incidentals(SimpleNode(4)) should (contain[Edge](SimpleNode(4), merged))
    graph.nodes should (containInAnyOrder[Node] (SimpleNode(3), SimpleNode(4), MergedNode(SimpleNode(1), SimpleNode(2))))
  }
}

object GraphTest {
  val containAll = new ContainAllWord
  val containInAnyOrder = new ContainInAnyOrderWord

  final class ContainAllWord {
    def apply[T](expectedElements: T*): Matcher[Traversable[T]] =
      new Matcher[Traversable[T]] {
        def apply(left: Traversable[T]): MatchResult =
          MatchResult(
            expectedElements.forall(expected => left.exists(e => e == expected)),
            left + " did not contain all expected elements " + expectedElements,
            left + " contained all expected elements " + expectedElements
          )
      }
  }

  final class ContainInAnyOrderWord {
    def apply[T](expectedElements: T*): Matcher[Traversable[T]] =
      new Matcher[Traversable[T]] {
        def apply(left: Traversable[T]): MatchResult =
          MatchResult(
            expectedElements.forall(expected => left.exists(e => e == expected)) && expectedElements.size == left.size,
            left + " did not contain all expected elements " + expectedElements,
            left + " contained all expected elements " + expectedElements
          )
      }
  }

}

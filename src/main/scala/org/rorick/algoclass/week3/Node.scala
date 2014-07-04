package org.rorick.algoclass.week3

/**
 * Model classes for graph nodes.
 */
sealed trait Node

case class SimpleNode(value: Int) extends Node {
  override val toString = "[" + value + "]"
}

case class MergedNode(values: Node*) extends Node {
  override val toString = values mkString("[", ",", "]")
}

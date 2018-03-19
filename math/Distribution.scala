package cse.fitzgero.mcts.math

trait Distribution[T] extends Any {
  def isEmpty: Boolean
  def min: Option[T]
  def max: Option[T]
  def mean: Option[T]
  def variance: Option[T]
  def standardDeviation: Option[T]
  def count: Int
}

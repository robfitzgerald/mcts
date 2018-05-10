package cse.bdlab.fitzgero.mcts.core.terminationcriterion

import java.time.Instant

import cse.bdlab.fitzgero.mcts.tree.MonteCarloTree

class DurationTermination [S,A,N <: MonteCarloTree[S,A,_,_,_,_]] (
  val computationTimeBudget: Long
) extends TerminationCriterion[S,A,N] {
  var startTime: Long = 0L
  def init(): Unit = {
    startTime = Instant.now.toEpochMilli
  }
  def withinComputationalBudget(monteCarloTree: N): Boolean = {
    Instant.now.toEpochMilli - startTime < computationTimeBudget
  }

  override def toString: String = {
    val endTimeShouldBe = startTime + computationTimeBudget
    s"DurationTermination startTime: $startTime computationalTimeBudget: $computationTimeBudget endTimeShouldBe: $endTimeShouldBe"
  }
}

object DurationTermination {
  def apply[S,A,N <: MonteCarloTree[S,A,_,_,_,_]](computationTimeBudget: Long): DurationTermination[S,A,N] =
    new DurationTermination[S,A,N](computationTimeBudget)
}

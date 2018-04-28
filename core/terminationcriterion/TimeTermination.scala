package cse.fitzgero.mcts.core.terminationcriterion

import java.time.Instant

import cse.fitzgero.mcts.tree.MonteCarloTree

class TimeTermination [S,A,N <: MonteCarloTree[S,A,_,_,_,_]] (
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
    s"TimeTermination startTime: $startTime computationalTimeBudget: $computationTimeBudget endTimeShouldBe: $endTimeShouldBe"
  }
}

object TimeTermination {
  def apply[S,A,N <: MonteCarloTree[S,A,_,_,_,_]](computationTimeBudget: Long): TimeTermination[S,A,N] =
    new TimeTermination[S,A,N](computationTimeBudget)
}

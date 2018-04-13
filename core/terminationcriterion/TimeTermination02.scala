package cse.fitzgero.mcts.core.terminationcriterion

import java.time.Instant

import cse.fitzgero.mcts.tree.MonteCarloTreeArbitraryUpdate

class TimeTermination02 [S,A,N <: MonteCarloTreeArbitraryUpdate[S,A,_,_,_,_]] (
  val computationTimeBudget: Long
) extends TerminationCriterion02[S,A,N] {
  var startTime: Long = 0L
  def init(): Unit = {
    startTime = Instant.now.toEpochMilli
  }
  def withinComputationalBudget(monteCarloTree: N): Boolean = {
    //    val now = Instant.now.toEpochMilli
    //    println(s"startTime: $startTime now: $now within computational budget: ${now - startTime < computationTimeBudget}")
    Instant.now.toEpochMilli - startTime < computationTimeBudget
  }

  override def toString: String = {
    val endTimeShouldBe = startTime + computationTimeBudget
    s"TimeTermination startTime: $startTime computationalTimeBudget: $computationTimeBudget endTimeShouldBe: $endTimeShouldBe"
  }
}

object TimeTermination02 {
  def apply[S,A,N <: MonteCarloTreeArbitraryUpdate[S,A,_,_,_,_]](computationTimeBudget: Long): TimeTermination02[S,A,N] =
    new TimeTermination02[S,A,N](computationTimeBudget)
}

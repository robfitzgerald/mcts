package cse.fitzgero.mcts.core.terminationcriterion

import cse.fitzgero.mcts.tree.MonteCarloTreeArbitraryUpdate

class IterationTermination [S,A,N <: MonteCarloTreeArbitraryUpdate[S,A,_,_,_,_]] (
  val iterationThreshold: Long
) extends TerminationCriterion02[S,A,N] {
  var iter = 0L
  def init(): Unit = {}
  def withinComputationalBudget(monteCarloTree: N): Boolean = {
    monteCarloTree.visits < iterationThreshold
  }

  override def toString: String = {
    s"IterationTermination iterationThreshold $iterationThreshold"
  }
}

object IterationTermination {
  def apply[S,A,N <: MonteCarloTreeArbitraryUpdate[S,A,_,_,_,_]](iterationThreshold: Long): IterationTermination[S,A,N] =
    new IterationTermination[S,A,N](iterationThreshold)
}
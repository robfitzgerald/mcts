package cse.fitzgero.mcts.core.terminationcriterion

import cse.fitzgero.mcts.tree._

trait TerminationCriterion02 [S,A,N <: MonteCarloTreeArbitraryUpdate[S,A,_,_,_,_]] {
  def init(): Unit
  def withinComputationalBudget(monteCarloTree: N): Boolean
}


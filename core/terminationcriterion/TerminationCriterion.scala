package cse.fitzgero.mcts.core.terminationcriterion

import cse.fitzgero.mcts.tree._

trait TerminationCriterion [S,A,N <: MonteCarloTree[S,A,_,_,_,_]] {
  def init(): Unit
  def withinComputationalBudget(monteCarloTree: N): Boolean
}


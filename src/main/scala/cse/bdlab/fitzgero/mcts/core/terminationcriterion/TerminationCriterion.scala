package cse.bdlab.fitzgero.mcts.core.terminationcriterion

import cse.bdlab.fitzgero.mcts.tree._

trait TerminationCriterion [S,A,N <: MonteCarloTree[S,A,_,_,_,_]] {
  def init(): Unit
  def withinComputationalBudget(monteCarloTree: N): Boolean
}


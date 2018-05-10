package cse.bdlab.fitzgero.mcts.core.terminationcriterion

import cse.bdlab.fitzgero.mcts.tree.MonteCarloTree

/**
  * the budget for this MCTS terminates at a specific time
  * @param endTime an Epoch time in milliseconds that we should interrupt this MCTS
  */
class TimeTermination [S,A,N <: MonteCarloTree[S,A,_,_,_,_]] (endTime: Long) extends TerminationCriterion[S,A,N] {
  /**
    * nothing to initialize
    */
  override def init(): Unit = {}

  /**
    * tests if the current time has surpassed the provided end time
    * @param monteCarloTree ignored
    * @return
    */
  override def withinComputationalBudget(monteCarloTree: N): Boolean = {
    System.currentTimeMillis < endTime
  }

  override def toString: String = s"TimeTermination endTime $endTime"
}

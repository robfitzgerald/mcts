package cse.fitzgero.mcts.example.mckdv.standardmcts

import cse.fitzgero.mcts.example.mckdv.implementation.MCKDV._
import cse.fitzgero.mcts.tree.MCTreeStandardReward

/**
  * Mutliple Choice Knapsack Problems with Dependent Weights
  */
class MCKDVStandardMCTSPedrosoReiSolver(val problem: Problem, val costBound: Int, val seed: Long = 0L, val timeBudget: Long = 5000L) extends MCKDVStandardMCTS {



  override def evaluateTerminal(state: Selection): Double = ??? // (costOfSelection(state, problem.dependencies) / maxPossibleCost).toDouble

  override def getSearchCoefficients(tree: MCTreeStandardReward[Selection, Choice]): Coefficients = ExplorationCoefficient

  override def getDecisionCoefficients(tree: MCTreeStandardReward[Selection, Choice]): Coefficients = SearchCoefficient
}

object MCKDVStandardMCTSPedrosoReiSolver {

  def apply(problem: Problem, costBound: Int): MCKDVStandardMCTSPedrosoReiSolver =
    new MCKDVStandardMCTSPedrosoReiSolver(problem, costBound)

  def apply(problem: Problem, costBound: Int, seed: Long, timeBudget: Long): MCKDVStandardMCTSPedrosoReiSolver =
    new MCKDVStandardMCTSPedrosoReiSolver(problem, costBound, seed, timeBudget)
}

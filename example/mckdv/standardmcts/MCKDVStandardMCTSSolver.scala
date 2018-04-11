package cse.fitzgero.mcts.example.mckdv.standardmcts

import cse.fitzgero.mcts.example.mckdv.implementation.MCKDV._
import cse.fitzgero.mcts.tree.MCTreeStandardReward
/**
  * Mutliple Choice Knapsack Problems with Dependent Weights
  */
class MCKDVStandardMCTSSolver (val problem: Problem, val costBound: Int, val seed: Long = 0L, val timeBudget: Long = 5000L) extends MCKDVStandardMCTS {

  // costBound * 2 set as a value that could dominate over any Choice costs and Dependency costs.
  // see costBound * 2 also in MCKDVGenerator
  val maxPossibleCost: BigDecimal = problem.multiset.size * problem.multiset.head.size * costBound * 2

  override def evaluateTerminal(state: Selection): Double = (costOfSelection(state, problem.dependencies) / maxPossibleCost).toDouble

  override def getSearchCoefficients(tree: MCTreeStandardReward[Selection, Choice]): Coefficients = ExplorationCoefficient

  override def getDecisionCoefficients(tree: MCTreeStandardReward[Selection, Choice]): Coefficients = SearchCoefficient
}

object MCKDVStandardMCTSSolver {
  def apply(problem: Problem, costBound: Int): MCKDVStandardMCTSSolver = new MCKDVStandardMCTSSolver(problem, costBound)
  def apply(problem: Problem, costBound: Int, seed: Long, timeBudget: Long): MCKDVStandardMCTSSolver = new MCKDVStandardMCTSSolver(problem, costBound, seed, timeBudget)
}
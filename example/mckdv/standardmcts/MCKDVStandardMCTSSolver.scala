package cse.fitzgero.mcts.example.mckdv.standardmcts

import cse.fitzgero.mcts.algorithm.samplingpolicy.scalar.UCTScalarStandardReward
import cse.fitzgero.mcts.example.mckdv.implementation.MCKDV._
/**
  * Mutliple Choice Knapsack Problems with Dependent Weights
  */
class MCKDVStandardMCTSSolver (val problem: Problem, val costBound: Int, val seed: Long = 0L, val timeBudget: Long = 5000L) extends MCKDVStandardMCTS {

  // costBound * 2 set as a value that could dominate over any Choice costs and Dependency costs.
  // see costBound * 2 also in MCKDVGenerator
  val maxPossibleCost: BigDecimal = problem.multiset.size * problem.multiset.head.size * costBound * 2

  override def evaluateTerminal(state: Selection): Update = (costOfSelection(state, problem.dependencies) / maxPossibleCost).toDouble

  override def getSearchCoefficients(tree: Tree): Coefficients = UCTScalarStandardReward.ExplorationCoefficient

  override def getDecisionCoefficients(tree: Tree): Coefficients = UCTScalarStandardReward.DecisionCoefficient

  // there is no update in standard MCTS
  override def updateSearchCoefficients(simulationResult: Double): Coefficients = UCTScalarStandardReward.ExplorationCoefficient
}

object MCKDVStandardMCTSSolver {
  def apply(problem: Problem, costBound: Int): MCKDVStandardMCTSSolver = new MCKDVStandardMCTSSolver(problem, costBound)
  def apply(problem: Problem, costBound: Int, seed: Long, timeBudget: Long): MCKDVStandardMCTSSolver = new MCKDVStandardMCTSSolver(problem, costBound, seed, timeBudget)
}
package cse.fitzgero.mcts.example.mckdv.standardmcts

import cse.fitzgero.mcts.algorithm.samplingpolicy.scalar.UCTScalarPedrosoReiReward._
import cse.fitzgero.mcts.example.mckdv.implementation.MCKDV._

/**
  * Mutliple Choice Knapsack Problems with Dependent Weights
  */
class MCKDVStandardMCTSPedrosoReiSolver(val problem: Problem, val costBound: Int, val seed: Long = 0L, val timeBudget: Long = 5000L) extends MCKDVPedrosoReiMCTS {

  override def updateSearchCoefficients(simulationResult: Update): Coefficients = ???

  override def evaluateTerminal(state: Selection): Update = costOfSelection(state, problem.dependencies)

  override def getSearchCoefficients(tree: Tree): Coefficients =
    Coefficients(ExplorationCoefficient, globalBestSimulation, globalWorstSimulation)

  override def getDecisionCoefficients(tree: Tree): Coefficients =
    Coefficients(SearchCoefficient, globalBestSimulation, globalWorstSimulation)
}

object MCKDVStandardMCTSPedrosoReiSolver {

  def apply(problem: Problem, costBound: Int): MCKDVStandardMCTSPedrosoReiSolver =
    new MCKDVStandardMCTSPedrosoReiSolver(problem, costBound)

  def apply(problem: Problem, costBound: Int, seed: Long, timeBudget: Long): MCKDVStandardMCTSPedrosoReiSolver =
    new MCKDVStandardMCTSPedrosoReiSolver(problem, costBound, seed, timeBudget)
}

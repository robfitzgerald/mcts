package cse.fitzgero.mcts.example.mckdv.pedrosorei

import cse.fitzgero.mcts.algorithm.samplingpolicy.banditfunction.UCT_PedrosoRei.{Maximize, Objective}
import cse.fitzgero.mcts.algorithm.samplingpolicy.scalar.UCTScalarPedrosoReiReward._
import cse.fitzgero.mcts.example.mckdv.implementation.MCKDV._

/**
  * Mutliple Choice Knapsack Problems with Dependent Weights
  */
class MCKDVPedrosoReiSolver(val problem: Problem, val seed: Long = 0L, val timeBudget: Long = 5000L, val objective: Objective = Maximize) extends MCKDVPedrosoReiMCTS {

  globalBestSimulation = objective.defaultBest
  globalWorstSimulation = objective.defaultWorst
  override var bestSolution: Selection = Set()

  override def evaluateTerminal(state: Selection): Update = costOfSelection(state, problem.dependencies)

  override def getSearchCoefficients(tree: Tree): Coefficients =
    Coefficients(ExplorationCoefficient, globalBestSimulation, globalWorstSimulation)

  override def getDecisionCoefficients(tree: Tree): Coefficients =
    Coefficients(SearchCoefficient, globalBestSimulation, globalWorstSimulation)
}

object MCKDVPedrosoReiSolver {

  def apply(problem: Problem): MCKDVPedrosoReiSolver =
    new MCKDVPedrosoReiSolver(problem)

  def apply(problem: Problem, seed: Long, timeBudget: Long): MCKDVPedrosoReiSolver =
    new MCKDVPedrosoReiSolver(problem, seed, timeBudget)

  def apply(problem: Problem, seed: Long, timeBudget: Long, objective: Objective): MCKDVPedrosoReiSolver =
    new MCKDVPedrosoReiSolver(problem, seed, timeBudget, objective)
}

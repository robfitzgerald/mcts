package cse.fitzgero.mcts.example.mckdv.pedrosorei

import cse.fitzgero.mcts.algorithm.samplingpolicy.banditfunction.UCT_PedrosoRei.{Maximize, Objective}
import cse.fitzgero.mcts.example.mckdv.implementation.{MCKDV, MCKDVGenerator}

class MCKDVPedrosoReiExperiment(val random: scala.util.Random, val costBound: Int, val objective: Objective) extends MCKDVGenerator {
    case class MCKDVExperimentResult(sumOptimalCostBaseline: BigDecimal, sumOptimalCostForComparisons: BigDecimal, sumSearchCost: BigDecimal, avgOptimalCost: BigDecimal, avgSearchCost: BigDecimal, completeSolutions: Int, optimalSolutions: Int, avgIterations: Long)
  def run(n: Int, k: Int, trials: Int, timeBudget: Long): MCKDVExperimentResult = {
    val (optCost, optCostForComparisons, searchCost, completeSolutions, optimalSolutions, iterations) = (1 to trials).map {
      m => {
        val (problem, optimal) = genProblem(n,k,objective)
        val optCostBaseline = MCKDV.costOfSelection(optimal, problem.dependencies)
        val solver = MCKDVPedrosoReiSolver(problem, 0, timeBudget, objective)
        val tree = solver.run()
        val iterations = tree.visits
        val searchCost = MCKDV.costOfSelection(solver.bestSolution, problem.dependencies)
        val completeSolution = if (solver.bestSolution.size == n) 1 else 0
        val optCostForComparisons = if (completeSolution == 1) optCostBaseline else BigDecimal(0)
        val gotItGood = if (optCostBaseline.toBigInt == searchCost.toBigInt) 1 else 0
        (optCostBaseline,optCostForComparisons,searchCost,completeSolution,gotItGood,iterations)
      }
    }.reduce {
      (a,b) => {
        (a._1+b._1,a._2+b._2,a._3+b._3,a._4+b._4,a._5+b._5,a._6+b._6)
      }
    }
    MCKDVExperimentResult(optCost, optCostForComparisons, searchCost, optCost/trials, searchCost/trials, completeSolutions, optimalSolutions, iterations/trials)
  }
}

object MCKDVPedrosoReiExperiment {
  def apply(random: scala.util.Random, costBound: Int, objective: Objective): MCKDVPedrosoReiExperiment = new MCKDVPedrosoReiExperiment(random, costBound, objective)
}
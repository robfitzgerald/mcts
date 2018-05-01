package cse.bdlab.fitzgero.mcts.example.mckdv.standardmcts

import cse.bdlab.fitzgero.mcts.algorithm.samplingpolicy.banditfunction.UCT_PedrosoRei.{Maximize, Objective}
import cse.bdlab.fitzgero.mcts.example.mckdv.implementation.{MCKDV, MCKDVGenerator}

class MCKDVStandardMCTSExperiment (val random: scala.util.Random, val costBound: Int) extends MCKDVGenerator {
  case class MCKDVExperimentResult(sumOptimalCost: BigDecimal, sumSearchCost: BigDecimal, avgOptimalCost: BigDecimal, avgSearchCost: BigDecimal)
  def run(n: Int, k: Int, trials: Int, timeBudget: Long): MCKDVExperimentResult = {
    val (optCost, searchCost) = (1 to trials).map {
      m => {
        val lowerBounds = BigDecimal(0)
        val upperBounds = BigDecimal(n * costBound * 2)
        val (problem, optimal) = genProblem(n,k,Maximize(lowerBounds,upperBounds))
        val optCost = MCKDV.costOfSelection(optimal, problem.dependencies)
        val solver = MCKDVStandardMCTSSolver(problem, costBound * 2, 0, timeBudget)
        val tree = solver.run()
//        println(s"Standard MCTS Tree search complete with ${tree.visits} iterations")
        val searchCost = MCKDV.costOfSelection(solver.bestGame(tree).toSet, problem.dependencies)
        (optCost,searchCost)
      }
    }.reduce {
      (a,b) => {
        (a._1+b._1,a._2+b._2)
      }
    }
    MCKDVExperimentResult(optCost, searchCost, optCost/trials, searchCost/trials)
  }
}

object MCKDVStandardMCTSExperiment {
  def apply(costBound: Int): MCKDVStandardMCTSExperiment = new MCKDVStandardMCTSExperiment(scala.util.Random, costBound)
  def apply(random: scala.util.Random, costBound: Int): MCKDVStandardMCTSExperiment = new MCKDVStandardMCTSExperiment(random, costBound)
}
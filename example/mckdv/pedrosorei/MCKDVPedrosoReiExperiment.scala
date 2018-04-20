package cse.fitzgero.mcts.example.mckdv.pedrosorei

import cse.fitzgero.mcts.algorithm.samplingpolicy.banditfunction.UCT_PedrosoRei.{Maximize, Objective}
import cse.fitzgero.mcts.example.mckdv.implementation.{MCKDV, MCKDVGenerator}

class MCKDVPedrosoReiExperiment(val random: scala.util.Random, val costBound: Int, val objective: Objective) extends MCKDVGenerator {
    case class MCKDVExperimentResult(sumOptimalCostBaseline: BigDecimal, sumOptimalCostForComparisons: BigDecimal, sumSearchCost: BigDecimal, avgOptimalCost: BigDecimal, avgSearchCost: BigDecimal, completeSolutions: Int, avgIterations: Long)
  def run(n: Int, k: Int, trials: Int, timeBudget: Long): MCKDVExperimentResult = {
    val (optCost, optCostForComparisons, searchCost, completeSolutions, iterations) = (1 to trials).map {
      m => {
        val (problem, optimal) = genProblem(n,k,objective)
        val optCostBaseline = MCKDV.costOfSelection(optimal, problem.dependencies)
        val solver = MCKDVPedrosoReiSolver(problem, 0, timeBudget, objective)
        val tree = solver.run()
        val iterations = tree.visits
        val searchCost = MCKDV.costOfSelection(solver.bestSolution, problem.dependencies)
        val completeSolution = if (solver.bestSolution.size == n) 1 else 0
        val optCostForComparisons = if (completeSolution == 1) optCostBaseline else BigDecimal(0)
//        println(s"problem $m")
//        println(problem)
//        println(optimal)
//        println(s"optimal $m")
//        println(tree.printTree(2))
//        println(s"Tree Search complete with ${tree.visits} iterations, optimal cost: $optCost; search cost: $searchCost")
//        println(s"best $m")
//        println(solver.bestGame(tree).toSet)
        (optCostBaseline,optCostForComparisons,searchCost,completeSolution,iterations)
      }
    }.reduce {
      (a,b) => {
        (a._1+b._1,a._2+b._2,a._3+b._3,a._4+b._4,a._5+b._5)
      }
    }
    MCKDVExperimentResult(optCost, optCostForComparisons, searchCost, optCost/trials, searchCost/trials, completeSolutions, iterations/trials)
  }
}

object MCKDVPedrosoReiExperiment {
  def apply(costBound: Int): MCKDVPedrosoReiExperiment = new MCKDVPedrosoReiExperiment(scala.util.Random, costBound, Maximize)
  def apply(random: scala.util.Random, costBound: Int): MCKDVPedrosoReiExperiment = new MCKDVPedrosoReiExperiment(random, costBound, Maximize)
  def apply(random: scala.util.Random, costBound: Int, objective: Objective): MCKDVPedrosoReiExperiment = new MCKDVPedrosoReiExperiment(random, costBound, objective)
}
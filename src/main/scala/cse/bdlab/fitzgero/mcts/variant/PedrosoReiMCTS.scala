package cse.bdlab.fitzgero.mcts.variant

import cse.bdlab.fitzgero.mcts.MonteCarloTreeSearch
import cse.bdlab.fitzgero.mcts.algorithm.backup.StandardBackup
import cse.bdlab.fitzgero.mcts.algorithm.bestchild.StandardBestChild
import cse.bdlab.fitzgero.mcts.algorithm.defaultpolicy.StandardDefaultPolicy
import cse.bdlab.fitzgero.mcts.algorithm.expand.StandardExpand
import cse.bdlab.fitzgero.mcts.algorithm.samplingpolicy.banditfunction.UCT_PedrosoRei.Objective
import cse.bdlab.fitzgero.mcts.algorithm.samplingpolicy.scalar.UCTScalarPedrosoReiReward
import cse.bdlab.fitzgero.mcts.algorithm.treepolicy.StandardTreePolicy
import cse.bdlab.fitzgero.mcts.tree._

trait PedrosoReiMCTS[S,A] extends MonteCarloTreeSearch[S,A]
                        with StandardBestChild[S,A]
                        with StandardTreePolicy[S,A]
                        with StandardDefaultPolicy[S,A]
                        with StandardBackup[S,A]
                        with StandardExpand[S,A] {

  def objective: Objective

  final override type Reward = Double
  final override type Update = BigDecimal
  final override type Coefficients = UCTScalarPedrosoReiReward.Coefficients

  // these are updated by updateSearchCoefficients in the implementing class
  // best and worst depend on maximizing vs. minimizing objective
  var globalBestSimulation: Update
  var globalWorstSimulation: Update

  /**
    * implementation should define this var, which should be used to track the best observed State
    */
  var bestSolution: S

  /**
    * added 20181128. count allocations of new nodes.
    */
  var nodesCreated: Long = 0

  final override def rewardOrdering: Ordering[Reward] = scala.math.Ordering.Double

  final override type Tree = MCTreePedrosoReiReward[S,A]

  final override def startNode(s: S): MCTreePedrosoReiReward[S, A] = MCTreePedrosoReiReward(s, None, objective = objective)

  final override def createNewNode(state: S, action: Option[A]): MCTreePedrosoReiReward[S, A] = {
    nodesCreated += 1
    MCTreePedrosoReiReward(state, action, objective)
  }


  final override def updateMetaData(simulationResult: Update, node: Tree, leafState: S): Coefficients = {
    if (objective.isWorseThan(simulationResult, globalWorstSimulation)) globalWorstSimulation = simulationResult
    if (objective.isBetterThanOrEqualTo(simulationResult, globalBestSimulation)) {
      bestSolution = leafState
      globalBestSimulation = simulationResult
    }
    UCTScalarPedrosoReiReward.Coefficients(getSearchCoefficients(node).Cp, globalBestSimulation, globalWorstSimulation)
  }
}


package cse.fitzgero.mcts.algorithm.samplingpolicy.scalar

import cse.fitzgero.mcts.MonteCarloTreeSearch
import cse.fitzgero.mcts.algorithm.samplingpolicy.banditfunction.UCT_PedrosoRei


trait UCTScalarPedrosoReiReward[S,A] extends MonteCarloTreeSearch[S,A] {
  self: {
    type Reward = Double
    type Coefficients = UCTScalarPedrosoReiReward.Coefficients
  } =>

  /**
    * Upper Confidence Bound For Trees sampling method
    * @param node the node to evaluate
    * @return
    */
  def evaluateBranch(node: Tree, c: Coefficients): Reward = node.reward
}

object UCTScalarPedrosoReiReward {

  /**
    * coefficients for the Pedroso Rei UCT algorithm. see UCT_PedrosoRei for details
    * @param Cp exploration parameter, typically in the range [0,1] for rewards in the same range
    * @param globalBestSimulation the best simulation value found so far
    * @param globalWorstSimulation the worst simulation value found so far
    */
  case class Coefficients (Cp: Double, globalBestSimulation: BigDecimal, globalWorstSimulation: BigDecimal)

  val ExplorationCoefficient = 0.707D
  val SearchCoefficient = 0D
}
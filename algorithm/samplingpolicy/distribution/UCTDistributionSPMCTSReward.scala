package cse.fitzgero.mcts.algorithm.samplingpolicy.distribution

import cse.fitzgero.mcts.MonteCarloTreeSearch
import cse.fitzgero.mcts.algorithm.samplingpolicy.banditfunction.SP_UCT
import cse.fitzgero.mcts.distribution.{DoublePrecisionDistribution, Observation, RunningDistribution}

trait UCTDistributionSPMCTSReward[S,A] extends MonteCarloTreeSearch[S,A] {
  self: {
    type Reward = RunningDistribution
    type Coefficients = UCTDistributionSPMCTSReward.Coefficients
  } =>


  /**
    * Upper Confidence Bound For Trees sampling method
    * @param node the node to evaluate
    * @return
    */
  final override def evaluateBranch(node: Tree, coefficients: Coefficients): Reward = {
    node.reward
//    val parentVisits: Long = node.parent() match {
//      case None => 0L
//      case Some(parent) => parent.visits
//    }
//    SP_UCT(
//      node.reward,
//      node.visits,
//      parentVisits,
//      coefficients.Cp,
//      coefficients.D
//    )
  }
}

object UCTDistributionSPMCTSReward {
  /**
    * coefficients for the SP_UCT algorithm
    * @param Cp exploration parameter, typically 1 over root 2, exploitation around 0.1, exploration around 1
    * @param D variance parameter, an integer. paper suggests exploitation value of 32, exploration value of 20000
    */
  case class Coefficients (Cp: Double, D: Double)

  // taken from M.P.D. Schadd et. al, 2008, for their single player game with rewards in the range [0,1]
  val Exploitation = Coefficients(0.1, 32)
  val Balanced = Coefficients(0.5, 10000)
  val Exploration = Coefficients(1, 20000)

}
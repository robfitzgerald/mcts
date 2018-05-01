package cse.bdlab.fitzgero.mcts.algorithm.samplingpolicy.distribution

import cse.bdlab.fitzgero.mcts.MonteCarloTreeSearch
import cse.bdlab.fitzgero.mcts.algorithm.samplingpolicy.banditfunction.UCT
import cse.bdlab.fitzgero.mcts.distribution.{DoublePrecisionDistribution, Observation}

trait UCTDistributionStandardReward[S,A] extends MonteCarloTreeSearch[S,A] {
  self: {
    type Reward = DoublePrecisionDistribution
  } =>

  /**
    * coefficient for the standard UCT algorithm
    * @param Cp exploration parameter, typically in the range [0,1] for rewards in the same range
    */
  case class Coefficients (Cp: Double = 0.7071D)

  /**
    * Upper Confidence Bound For Trees sampling method
    * @param node the node to evaluate
    * @return
    */
  override def evaluateBranch(node: Tree, c: Coefficients): Reward = {
    val parentVisits: Long = node.parent() match {
      case None => 0L
      case Some(parent) => parent.visits
    }
    val reward: Double = node.reward(c).mean match {
      case None => 0D
      case Some(mean) =>
        // UCT will divide by count, but this is already a mean, so we will artificially inflate this reward value to offset that calculation
        mean * node.visits
    }
    val uct = UCT(
      reward,
      node.visits,
      parentVisits,
      c.Cp)
    Observation(uct)
  }
}
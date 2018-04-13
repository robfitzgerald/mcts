package cse.fitzgero.mcts.variant

import cse.fitzgero.mcts.MonteCarloTreeSearch
import cse.fitzgero.mcts.algorithm.backup.Backup
import cse.fitzgero.mcts.algorithm.bestchild.StandardBestChild
import cse.fitzgero.mcts.algorithm.defaultpolicy.DefaultPolicy
import cse.fitzgero.mcts.algorithm.expand.StandardExpand
import cse.fitzgero.mcts.algorithm.samplingpolicy.distribution.UCTDistributionSPMCTSReward
import cse.fitzgero.mcts.algorithm.treepolicy.StandardTreePolicy
import cse.fitzgero.mcts.distribution.{Observation, RunningDistribution}
import cse.fitzgero.mcts.tree._

trait RewardDistributionSPMCTS[S,A] extends MonteCarloTreeSearch[S,A]
                                  with StandardBestChild[S,A]
                                  with StandardTreePolicy[S,A]
                                  with DefaultPolicy[S,A]
                                  with Backup[S,A]
                                  with StandardExpand[S,A] {

  final override type Reward = RunningDistribution
  final override type Coefficients = UCTDistributionSPMCTSReward.Coefficients
  final override type Update = Observation

  override val rewardOrdering: Ordering[RunningDistribution] = RunningDistribution.distributionByMeanOrdering

  final override type Tree = MCTreeWithDistribution[S,A]

  final override def startNode(s: S): MCTreeWithDistribution[S, A] = MCTreeWithDistribution(s)

  final override def createNewNode(state: S, action: Option[A]): MCTreeWithDistribution[S, A] =
    MCTreeWithDistribution[S,A](state, action)
}


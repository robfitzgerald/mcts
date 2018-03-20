package cse.fitzgero.mcts.variant

import cse.fitzgero.mcts.MonteCarloTreeSearch
import cse.fitzgero.mcts.algorithm.backup.StandardBackup
import cse.fitzgero.mcts.algorithm.bestchild.StandardBestChild
import cse.fitzgero.mcts.algorithm.defaultpolicy.StandardDefaultPolicy
import cse.fitzgero.mcts.algorithm.expand.StandardExpand
import cse.fitzgero.mcts.algorithm.samplingpolicy.distribution.UCTDistributionSPMCTSReward
import cse.fitzgero.mcts.algorithm.treepolicy.StandardTreePolicy
import cse.fitzgero.mcts.distribution.DoublePrecisionDistribution
import cse.fitzgero.mcts.tree._

trait RewardDistributionSPMCTS[S,A] extends MonteCarloTreeSearch[S,A]
                                  with StandardBestChild[S,A]
                                  with StandardTreePolicy[S,A]
                                  with StandardDefaultPolicy[S,A]
                                  with StandardBackup[S,A]
                                  with StandardExpand[S,A]
                                  with UCTDistributionSPMCTSReward[S,A] {

  final override type Reward = DoublePrecisionDistribution

  override val rewardOrdering: Ordering[DoublePrecisionDistribution] = DoublePrecisionDistribution.distributionByMeanOrdering

  final override type Tree = MCTreeWithDistribution[S,A]

  final override def startNode(s: S): MCTreeWithDistribution[S, A] = MCTreeWithDistribution(s)

  final override def createNewNode(state: S, action: Option[A]): MCTreeWithDistribution[S, A] =
    MCTreeWithDistribution[S,A](state, action)
}


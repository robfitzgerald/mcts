package cse.bdlab.fitzgero.mcts.variant

import cse.bdlab.fitzgero.mcts.MonteCarloTreeSearch
import cse.bdlab.fitzgero.mcts.algorithm.backup.StandardBackup
import cse.bdlab.fitzgero.mcts.algorithm.bestchild.StandardBestChild
import cse.bdlab.fitzgero.mcts.algorithm.defaultpolicy.StandardDefaultPolicy
import cse.bdlab.fitzgero.mcts.algorithm.expand.StandardExpand
import cse.bdlab.fitzgero.mcts.algorithm.samplingpolicy.distribution.UCTDistributionSPMCTSReward
import cse.bdlab.fitzgero.mcts.algorithm.treepolicy.StandardTreePolicy
import cse.bdlab.fitzgero.mcts.distribution.{Observation, RunningDistribution}
import cse.bdlab.fitzgero.mcts.tree._

trait RewardDistributionSPMCTS[S,A] extends MonteCarloTreeSearch[S,A]
                                  with StandardBestChild[S,A]
                                  with StandardTreePolicy[S,A]
                                  with StandardDefaultPolicy[S,A]
                                  with StandardBackup[S,A]
                                  with StandardExpand[S,A] {

  final override type Reward = RunningDistribution
  final override type Coefficients = UCTDistributionSPMCTSReward.Coefficients
  final override type Update = Observation

  override val rewardOrdering: Ordering[RunningDistribution] = RunningDistribution.distributionByMeanOrdering

  final override type Tree = MCTreeWithDistribution[S,A]

  final override def startNode(s: S): MCTreeWithDistribution[S, A] = MCTreeWithDistribution(s)

  final override def createNewNode(state: S, action: Option[A]): MCTreeWithDistribution[S, A] =
    MCTreeWithDistribution[S,A](state, action)

  override def updateMetaData(simulationResult: Update, node: Tree, state: S): Coefficients = getSearchCoefficients(node)
}


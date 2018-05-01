package cse.bdlab.fitzgero.mcts.variant

import cse.bdlab.fitzgero.mcts.MonteCarloTreeSearch
import cse.bdlab.fitzgero.mcts.algorithm.backup.StandardBackup
import cse.bdlab.fitzgero.mcts.algorithm.bestchild.StandardBestChild
import cse.bdlab.fitzgero.mcts.algorithm.defaultpolicy.StandardDefaultPolicy
import cse.bdlab.fitzgero.mcts.algorithm.expand.StandardExpand
import cse.bdlab.fitzgero.mcts.algorithm.samplingpolicy.scalar.UCTScalarStandardReward
import cse.bdlab.fitzgero.mcts.algorithm.treepolicy.StandardTreePolicy
import cse.bdlab.fitzgero.mcts.tree._

trait StandardMCTS[S,A] extends MonteCarloTreeSearch[S,A]
                        with StandardBestChild[S,A]
                        with StandardTreePolicy[S,A]
                        with StandardDefaultPolicy[S,A]
                        with StandardBackup[S,A]
                        with StandardExpand[S,A] {

  final override type Reward = Double
  final override type Update = Double
  final override type Coefficients = UCTScalarStandardReward.Coefficients

  final override def rewardOrdering: Ordering[Double] = scala.math.Ordering.Double

  final override type Tree = MCTreeStandardReward[S,A]

  final override def startNode(s: S): MCTreeStandardReward[S, A] = MCTreeStandardReward(s)

  final override def createNewNode(state: S, action: Option[A]): MCTreeStandardReward[S, A] =
    MCTreeStandardReward(state, action)

  override def updateMetaData(simulationResult: Double, node: Tree, state: S): Coefficients = getSearchCoefficients(node)
}


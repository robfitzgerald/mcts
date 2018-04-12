package cse.fitzgero.mcts.variant

import cse.fitzgero.mcts.MonteCarloTreeSearch
import cse.fitzgero.mcts.algorithm.backup.Backup
import cse.fitzgero.mcts.algorithm.bestchild.StandardBestChild
import cse.fitzgero.mcts.algorithm.defaultpolicy.DefaultPolicy
import cse.fitzgero.mcts.algorithm.expand.StandardExpand
import cse.fitzgero.mcts.algorithm.samplingpolicy.scalar.UCTScalarStandardReward
import cse.fitzgero.mcts.algorithm.treepolicy.StandardTreePolicy
import cse.fitzgero.mcts.tree._

trait StandardMCTS[S,A] extends MonteCarloTreeSearch[S,A]
                        with StandardBestChild[S,A]
                        with StandardTreePolicy[S,A]
                        with DefaultPolicy[S,A]
                        with Backup[S,A]
                        with StandardExpand[S,A]
                        with UCTScalarStandardReward[S,A] {

  final override type Reward = Double
  final override type Update = Double
  final override type Coefficients = UCTScalarStandardReward.Coefficients

  final override def rewardOrdering: Ordering[Double] = scala.math.Ordering.Double

  final override type Tree = MCTreeStandardReward[S,A]

  final override def startNode(s: S): MCTreeStandardReward[S, A] = MCTreeStandardReward(s)

  final override def createNewNode(state: S, action: Option[A]): MCTreeStandardReward[S, A] =
    MCTreeStandardReward(state, action)
}


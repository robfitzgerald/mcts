package cse.fitzgero.mcts.variant

import cse.fitzgero.mcts.MonteCarloTreeSearchPedrosoRei
import cse.fitzgero.mcts.algorithm.backup.BackupPedrosoRei
import cse.fitzgero.mcts.algorithm.bestchild.StandardBestChild
import cse.fitzgero.mcts.algorithm.defaultpolicy.DefaultPolicyPedrosoRei
import cse.fitzgero.mcts.algorithm.expand.StandardExpand
import cse.fitzgero.mcts.algorithm.samplingpolicy.scalar.UCTScalarPedrosoReiReward
import cse.fitzgero.mcts.algorithm.treepolicy.StandardTreePolicy
import cse.fitzgero.mcts.tree._

trait PedrosoReiMCTS[S,A] extends MonteCarloTreeSearchPedrosoRei[S,A]
                        with StandardBestChild[S,A]
                        with StandardTreePolicy[S,A]
                        with DefaultPolicyPedrosoRei[S,A]
                        with BackupPedrosoRei[S,A]
                        with StandardExpand[S,A]
                        with UCTScalarPedrosoReiReward[S,A] {

  final override type Reward = Double

  final override def rewardOrdering: Ordering[Reward] = scala.math.Ordering.Double

  final override type Tree = MCTreePedrosoReiReward[S,A]

  final override def startNode(s: S): MCTreePedrosoReiReward[S, A] = MCTreePedrosoReiReward(s)

  final override def createNewNode(state: S, action: Option[A]): MCTreePedrosoReiReward[S, A] =
    MCTreePedrosoReiReward(state, action)
}


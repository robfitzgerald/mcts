package cse.fitzgero.mcts.tree

import cse.fitzgero.mcts.algorithm.samplingpolicy.banditfunction.SP_UCT
import cse.fitzgero.mcts.algorithm.samplingpolicy.distribution.UCTDistributionSPMCTSReward.Coefficients
import cse.fitzgero.mcts.distribution._

/**
  * standard Monte Carlo Tree, storing a Reward as a Double.
 *
  * @param action the (optional) action used to create this node
  * @param state the state associated with this node, where action was applied to the parent state to produce the current state
  * @tparam S a state type for our state space search
  * @tparam A an action type for transitions between states
  */
class MCTreeWithDistribution[S, A] (
  override val action: Option[A],
  override val state: S
)extends MonteCarloTreeArbitraryUpdate [S,A,RunningDistribution,Observation,Coefficients,MCTreeWithDistribution[S, A]]{
  var reward: RunningDistribution = RunningDistribution()

  override def update(simulationResult: Observation, c: Coefficients): Unit = {
    visits += 1
    reward = reward + simulationResult
    val parentVisits: Long = parent() match {
    case None => 0L
    case Some(p) => p.visits
    }
//    SP_UCT(
//          reward,
//          visits,
//          parentVisits,
//          c.Cp,
//          c.D
//        )
    //    case obs: Observation =>

//      reward = UCT(
//        averageReward,
//        visits,
//        parentVisits,
//        coefficients.Cp
//      )
//    case _ => ()
  }
}

object MCTreeWithDistribution {
  def apply[S,A](state: S): MCTreeWithDistribution[S,A] = new MCTreeWithDistribution[S,A](state = state, action = None)
  def apply[S,A](state: S, action: Option[A]): MCTreeWithDistribution[S,A] = {
    new MCTreeWithDistribution(state = state, action = action)
  }
}

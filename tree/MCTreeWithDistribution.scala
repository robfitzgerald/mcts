package cse.fitzgero.mcts.tree

import cse.fitzgero.mcts.math._

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
)extends MonteCarloTree [S,A,DoublePrecisionDistribution,MCTreeWithDistribution[S, A]]{
  var reward: DoublePrecisionDistribution = RunningDistribution()
  override def update(reward: DoublePrecisionDistribution): Unit = reward match {
    case obs: Observation =>
      updateReward((r: DoublePrecisionDistribution) =>
        r match {
          case dist: RunningDistribution => Some(dist + obs)
          case storedObs: Observation => Some(RunningDistribution() + storedObs + obs) // shouldn't happen
        })
    case _ => ()
  }
}

object MCTreeWithDistribution {
  def apply[S,A](state: S): MCTreeWithDistribution[S,A] = new MCTreeWithDistribution[S,A](state = state, action = None)
  def apply[S,A](state: S, action: Option[A]): MCTreeWithDistribution[S,A] = {
    new MCTreeWithDistribution(state = state, action = action)
  }
}

package cse.fitzgero.mcts.tree

import cse.fitzgero.mcts.algorithm.samplingpolicy.banditfunction.UCT
import cse.fitzgero.mcts.algorithm.samplingpolicy.scalar.UCTScalarStandardReward

/**
  * standard Monte Carlo Tree, storing a Reward as a Double.
 *
  * @param action the (optional) action used to create this node
  * @param state the state associated with this node, where action was applied to the parent state to produce the current state
  * @tparam S a state type for our state space search
  * @tparam A an action type for transitions between states
  */
class MCTreeStandardReward[S, A] (
  override val action: Option[A],
  override val state: S
)extends MonteCarloTree [S,A,Double,Double,UCTScalarStandardReward.Coefficients,MCTreeStandardReward[S, A]] {

  var reward: Double = 0D

  protected var rewardSum: Double = 0D

  override def reward(coefficients: UCTScalarStandardReward.Coefficients): Double = {
    val parentVisits: Long = parent() match {
      case None => 0L
      case Some(p) => p.visits
    }
    reward = UCT(
      averageReward,
      visits,
      parentVisits,
      coefficients.Cp
    )
    reward
  }

  /**
    * updates the node information related to UCT for combinatorial search
    * @param simulationResult the data that we use to update this node
    * @param coefficients global best/worst simulations (must be set in tree search) and the Cp value
    */
  override def update(simulationResult: Double, coefficients: UCTScalarStandardReward.Coefficients): Unit = {
    visits += 1
    rewardSum += simulationResult
  }
  def averageReward: Double = if (visits == 0) 0D else rewardSum / visits

  override def treeSpecificPrintData: String = f"$reward%.2f reward from $visits visits"
}

object MCTreeStandardReward {
  def apply[S,A](state: S): MCTreeStandardReward[S,A] = new MCTreeStandardReward[S,A](state = state, action = None)
  def apply[S,A](state: S, action: Option[A]): MCTreeStandardReward[S,A] = {
    new MCTreeStandardReward(state = state, action = action)
  }
}
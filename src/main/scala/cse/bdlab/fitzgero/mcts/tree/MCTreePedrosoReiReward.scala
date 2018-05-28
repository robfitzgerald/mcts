package cse.bdlab.fitzgero.mcts.tree

import cse.bdlab.fitzgero.mcts.algorithm.samplingpolicy.banditfunction.UCT_PedrosoRei
import cse.bdlab.fitzgero.mcts.algorithm.samplingpolicy.banditfunction.UCT_PedrosoRei.{Maximize, Objective}
import cse.bdlab.fitzgero.mcts.algorithm.samplingpolicy.scalar.UCTScalarPedrosoReiReward

/**
  * Monte Carlo Tree, storing a Reward as the best (by min or max) simulation result
  *
  * @param action the (optional) action used to create this node
  * @param state the state associated with this node, where action was applied to the parent state to produce the current state
  * @tparam S a state type for our state space search
  * @tparam A an action type for transitions between states
  */
class MCTreePedrosoReiReward[S, A] (
  override val action: Option[A],
  override val state: S,
  objective: Objective
) extends MonteCarloTree [S,A,Double,BigDecimal,UCTScalarPedrosoReiReward.Coefficients,MCTreePedrosoReiReward[S, A]] with Serializable {

  /**
    * stores the most recently calculated reward value for this node
    */
  private var storedReward: Double = 0D

  /**
    * calculate the reward value for this node
    * @param coefficients the global coefficients required to calculate this
    * @return
    */
  def reward(coefficients: UCTScalarPedrosoReiReward.Coefficients): Double = {
    val parentVisits: Long = parent() match {
      case None => visits // reward evaluation of the root will eval exploration as log(1/1) == 0D.
      case Some(p) => p.visits
    }
    storedReward = UCT_PedrosoRei(
      coefficients.globalBestSimulation,
      coefficients.globalWorstSimulation,
      bestSimulation,
      averageSimulation,
      visits,
      parentVisits,
      coefficients.Cp
    )
    storedReward
  }

  /**
    * holds the best simulation value that this node has seen
    */
  var bestSimulation: BigDecimal = objective.defaultSimulation

  /**
    * accumulated simulation results at this node. used to calculate averageSimulation
    */
  protected var averageSum: BigDecimal = BigDecimal.decimal(0)

  /**
    * updates the node information related to UCT for combinatorial search
    * @param simulationResult the data that we use to update this node
    * @param coefficients global best/worst simulations (must be set in tree search) and the Cp value
    */
  override def update(simulationResult: BigDecimal, coefficients: UCTScalarPedrosoReiReward.Coefficients): Unit = {
    visits += 1
    averageSum += simulationResult
    if (objective.isBetterThanOrEqualTo(simulationResult, bestSimulation)) bestSimulation = simulationResult
  }

  /**
    * calculates the average simulation value for this node
    * @return
    */
  def averageSimulation: BigDecimal = if (visits == 0) BigDecimal.decimal(0) else averageSum / visits

  override def treeSpecificPrintData: String = f"$storedReward%.2f reward from $visits visits"
}


object MCTreePedrosoReiReward extends Serializable {
  def apply[S,A](state: S, action: Option[A], objective: Objective): MCTreePedrosoReiReward[S,A] =
    new MCTreePedrosoReiReward(state = state, action = action, objective = objective)
}
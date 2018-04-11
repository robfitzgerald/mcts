package cse.fitzgero.mcts.tree

import cse.fitzgero.mcts.algorithm.samplingpolicy.banditfunction.UCT_PedrosoRei
import cse.fitzgero.mcts.algorithm.samplingpolicy.scalar.UCTScalarPedrosoReiReward

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
  override val state: S
) extends MonteCarloTreeArbitraryUpdate [S,A,Double,BigDecimal,UCTScalarPedrosoReiReward.Coefficients,MCTreePedrosoReiReward[S, A]] {
  var reward: Double = 0D
  var bestSimulation: BigDecimal = BigDecimal.decimal(0)
  protected var averageSum: BigDecimal = BigDecimal.decimal(0)

  /**
    * updates the node information related to UCT for combinatorial search
    * @param simulationResult the data that we use to update this node
    * @param coefficients global best/worst simulations (must be set in tree search) and the Cp value
    */
  override def update(simulationResult: BigDecimal, coefficients: UCTScalarPedrosoReiReward.Coefficients): Unit = {
    visits += 1
    averageSum += simulationResult
    if (simulationResult > bestSimulation) bestSimulation = simulationResult
    val parentVisits: Long = parent() match {
      case None => 0L
      case Some(p) => p.visits
    }
    reward = UCT_PedrosoRei(
      coefficients.globalBestSimulation,
      coefficients.globalWorstSimulation,
      bestSimulation,
      averageSimulation,
      visits,
      parentVisits,
      coefficients.Cp
    )
  }

  /**
    * calculates the average simulation value for this node
    * @return
    */
  def averageSimulation: BigDecimal = averageSum / visits
}


object MCTreePedrosoReiReward {

  val minimize: (BigDecimal, BigDecimal) => BigDecimal = (a: BigDecimal, b: BigDecimal) => if (a < b) a else b
  val maximize: (BigDecimal, BigDecimal) => BigDecimal = (a: BigDecimal, b: BigDecimal) => if (a > b) a else b

  def apply[S,A](state: S): MCTreePedrosoReiReward[S,A] =
    new MCTreePedrosoReiReward[S,A](state = state, action = None)

  def apply[S,A](state: S, action: Option[A]): MCTreePedrosoReiReward[S,A] =
    new MCTreePedrosoReiReward(state = state, action = action)
}
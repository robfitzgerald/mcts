package cse.bdlab.fitzgero.mcts.model.value

import scala.collection.BitSet

import cse.bdlab.fitzgero.mcts.model.state.combinatorial.MultiChoiceState

/**
  * holds the global state of a value function
  * @tparam S type parameter for the state representation
  */
sealed trait ValueFunction [S] {
  def updateGlobalState(obs: Double, state: S): Unit
}

object ValueFunction {

  /**
    * a Pedroso-Rei value function is used to model combinatorial search problems
    * in the context of a Monte Carlo Tree Search. it uses the modified UCT function
    * found in [[cse.bdlab.fitzgero.mcts.model.value.costfunction.UCTPedrosoRei]] which
    * relies on tracking the global min and max observations in order to fit the arbitrary
    * combinatorial costs to a value range which is meaningful to the UCB1 function.
    *
    * @param Cp coefficient to use while exploring the tree. $\sqrt{2}$ has been
    *                      shown for many problems to effectively manage the tradeoff between
    *                      exploration and exploitation. zero means pure exploitation.
    * @param globalMinObservation search-wide minimum cost observation
    * @param globalMaxObservation search-wide maximum cost observation
    * @param globalMinState the state representation with the minimum cost observation
    */
  final class PedrosoRei(
    val Cp: Double = math.sqrt(2),
    var globalMinObservation: Double = Double.PositiveInfinity,
    var globalMaxObservation: Double = Double.NegativeInfinity,
    var globalMinState                     : MultiChoiceState = BitSet.empty
  ) extends ValueFunction[MultiChoiceState] {

    def updateGlobalState(obs: Double, state: MultiChoiceState): Unit = {
      if (obs < this.globalMinObservation) {
        this.globalMinObservation = obs
        this.globalMinState = state
      }
      if (obs > this.globalMaxObservation) this.globalMaxObservation = obs
    }
  }
}

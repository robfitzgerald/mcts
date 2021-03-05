package cse.bdlab.fitzgero.mcts.model.value

import cse.bdlab.fitzgero.mcts.model.observation.Observation
import cse.bdlab.fitzgero.mcts.model.tree.NewTree
import cse.bdlab.fitzgero.mcts.model.observation.ObservationOps._
import cse.bdlab.fitzgero.mcts.model.value.costfunction.UCTPedrosoRei

object ValueFunctionOps {

  implicit class ValueFunctionOpsInstance(vfn: ValueFunction[_]) {

    /**
      * use the value function to calculate the value of the child node
      *
      * @param parentObservations optional parent node observations, is None when the child is the root node
      * @param childObservations the observations for the node to calculate a new value for
      * @return the value of the child node
      */
    def calculateValue(
      parentObservations: Option[Observation],
      childObservations: Observation
    ): Double = {
      vfn match {
        case rei: ValueFunction.PedrosoRei =>

          val parentVisits: Int = parentObservations
            .map { _.visits }
            .getOrElse(childObservations.visits)
          val childVisits: Int = childObservations.visits

          val value = UCTPedrosoRei.apply(
            rei.globalMinObservation,
            rei.globalMaxObservation,
            childObservations.min,
            childObservations.mean,
            childVisits,
            parentVisits,
            rei.Cp
          )

          value
      }
    }
  }
}

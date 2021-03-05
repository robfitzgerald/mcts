package cse.bdlab.fitzgero.mcts.alg2

import cse.bdlab.fitzgero.mcts.model.tree.NewTree
import cse.bdlab.fitzgero.mcts.model.tree.NewTreeOps._
import cse.bdlab.fitzgero.mcts.model.value.ValueFunction

object Backup {

  /**
    * update the observations and value functions for all nodes along the provided path
    * @param path a path through the search tree, from the root
    * @param observation the observed cost sampled from this path, used to update all
    *                    nodes along the path
    * @param vfn function used to calculate the value of a node due to its observations
    */
  def apply[S](path: List[NewTree], observation: Double, observedState: S, vfn: ValueFunction[S]): Unit = {
    if (path.isEmpty) throw new IllegalArgumentException("backup called on empty list")
    else {
      // first, update the global values, if any
      vfn.updateGlobalState(observation, observedState)
      for { newTree <- path } { newTree.updateObservations(observation) }
    }
  }
}

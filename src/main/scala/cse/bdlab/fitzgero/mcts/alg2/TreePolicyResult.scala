package cse.bdlab.fitzgero.mcts.alg2

import cse.bdlab.fitzgero.mcts.model.tree.NewTree

/**
  * result of running a tree policy
  * @param path the path from the root to the selected node
  * @param selectedNode the selected node
  * @param depth index (from 0) of the selected node
  */
final case class TreePolicyResult(
  path: List[NewTree],
  selectedNode: NewTree,
  depth: Int
)

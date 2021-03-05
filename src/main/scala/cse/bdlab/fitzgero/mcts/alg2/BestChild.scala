package cse.bdlab.fitzgero.mcts.alg2

import cse.bdlab.fitzgero.mcts.model.tree.NewTree
import cse.bdlab.fitzgero.mcts.model.tree.NewTreeOps._
import cse.bdlab.fitzgero.mcts.model.value.ValueFunction
import cse.bdlab.fitzgero.mcts.model.value.ValueFunctionOps._

final case class BestChild(action: Int, node: NewTree)

object BestChild {

  /**
    * finds the best child of a tree node as ranked by the value function
    * @param newTree the tree node parent
    * @param vfn the value function
    * @return the best child, or, None if no child exists
    */
  def run[S](newTree: NewTree, vfn: ValueFunction[S]): Option[BestChild] = {
    newTree match {
      case _: NewTree.Leaf =>
        // trivial case, leaves have no children
        None
      case branch: NewTree.Branch1 =>
        // trivial case, Branch1 has exactly one child
        Some(BestChild(branch.childAction, branch.child))
      case n: NewTree.BranchN =>
        // rank the children and return only the highest-ranking child, if it exists
        val ranked: Array[(Double, BestChild)] =
          n.children.zipWithIndex.collect {
            case (Some(child), action) =>
              (vfn.calculateValue(Some(newTree.observations), child.observations), BestChild(action, child))
          }
        val result: Option[BestChild] =
          ranked
            .sortBy { case (rank, _) => -rank }
            .headOption
            .map {
              case (_, bestChildData) => bestChildData
            }
        result
    }
  }
}

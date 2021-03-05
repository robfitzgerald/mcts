package cse.bdlab.fitzgero.mcts.alg2

import scala.annotation.tailrec

import cats.Monoid

import cse.bdlab.fitzgero.mcts.model.observation.Observation
import cse.bdlab.fitzgero.mcts.model.state.combinatorial.{MultiChoice, MultiChoiceState}
import cse.bdlab.fitzgero.mcts.model.tree.NewTree
import cse.bdlab.fitzgero.mcts.model.tree.NewTreeOps._
import cse.bdlab.fitzgero.mcts.model.state.combinatorial.MultiChoiceStateOps._
import cse.bdlab.fitzgero.mcts.model.value.ValueFunction

/**
  * runs a tree policy for a combinatorial search state problem
  * @param multiChoice describes this multi-choice problem
  * @param expandSelectionFunction takes the tree depth and set of unexplored actions at this depth and
  *                          chooses the next action to expand
  */
final case class TreePolicyCombinatorial(
  multiChoice      : MultiChoice,
  expandSelectionFunction: (Int, Set[Int]) => Int
) {
  val maxDepth: Int = multiChoice.slots

  /**
    * Tree Policy: Select or create a leaf node from the
    * nodes already contained within the search tree (selection and expansion).
    *
    * function TREEPOLICY(v) while v is nonterminal do
    *   if v not fully expanded then return EXPAND(v)
    *   else v ← BESTCHILD(v, Cp) return v
    */
  def run[O <: Observation](root: NewTree, vfn: ValueFunction[MultiChoiceState])(
    implicit m: Monoid[O]
  ): TreePolicyResult = {

    @tailrec
    def _run(
      parent: Option[PreviousNodeAndTransitionAction],
      node: NewTree,
      depth: Int,
      solution: List[NewTree]
    ): (List[NewTree], Int) = {
      if (depth == maxDepth) (solution, depth)
      else {
        val maxActions        = multiChoice.actions(depth)
        val unexploredActions = unexploredActionsOf(node, maxActions)
        if (unexploredActions.isEmpty) {
          // we have explored all possible actions from this node,
          // so, we just have to pick the "best" one at this time
          BestChild.run(node, vfn) match {
            case None =>
              // bottomed out, end search and return path
              (solution, depth)
            case Some(bestChild) =>
              // update state and recurse
              val updatedSolution: List[NewTree] = bestChild.node +: solution
              val prevNodeAct: PreviousNodeAndTransitionAction =
                PreviousNodeAndTransitionAction(node, bestChild.action)
              _run(Some(prevNodeAct), bestChild.node, depth + 1, updatedSolution)
          }
        } else {
          // select a child state to create and attach it to the tree and also to the path,
          // ending the tree policy search
          val selectedAction = expandSelectionFunction(depth, unexploredActions)
          val stateWithActionAdded: MultiChoiceState =
            node.state.addChoiceToState(multiChoice.statePartitionOffsets, depth, selectedAction)

          val child                = new NewTree.Leaf(stateWithActionAdded, m.empty)
          val updatedNode: NewTree = node.addChild(parent, child, selectedAction, maxActions)

          val path: List[NewTree] = {
            if (solution.isEmpty) {
              // empty path solution, no need to consider modification to the root
              child +: Nil
            } else {
              // re-add node along with child to the solution path
              // (in case we had to modify this node's object ref)
              child +: (updatedNode +: solution.tail)
            }
          }

          (path, depth)
        }
      }
    }

    val (searchResult: List[NewTree], depth: Int) = _run(None, root, 0, List.empty)
    val path = root +: searchResult.reverse
    val selectedNode: NewTree = searchResult.headOption.getOrElse(root)

    TreePolicyResult(path, selectedNode, depth)
  }

  /**
    * create the list of actions that haven't been explored yet from this tree node.
    *
    * @param newTree the tree node
    * @param actions the number of actions possible at this node
    * @return the set of unexplored actions
    */
  def unexploredActionsOf(newTree: NewTree, actions: Int): Set[Int] = {
    val possibleActions: Set[Int] = (0 until actions).toSet
    val exploredActions: Set[Int] = newTree match {
      case _: NewTree.Leaf => Set.empty[Int]
      case branch1: NewTree.Branch1 =>
        Set(branch1.childAction)
      case branch: NewTree.BranchN =>
        branch.children.zipWithIndex
          .foldLeft(Set.empty[Int]) {
            case (acc, (child, idx)) => if (child.isDefined) acc + idx else acc
          }
    }
    possibleActions.diff(exploredActions)
  }
}

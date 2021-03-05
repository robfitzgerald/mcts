package cse.bdlab.fitzgero.mcts.model.tree

import scala.collection.BitSet

import cats.kernel.Monoid

import cse.bdlab.fitzgero.mcts.model.observation.Observation
import cse.bdlab.fitzgero.mcts.model.state.combinatorial.MultiChoiceState

/**
  * a mutable Monte Carlo Search Tree ADT
  */
sealed trait NewTree

object NewTree {

  /**
    * creates the root of a tree. roots begin as a fully-formed BranchN instance,
    * with no observations and no children.
    *
    * since NewTreeOps do not have the original object reference in scope to modify them
    * as is the case for other nodes, which can be promoted through the types Leaf, Branch1,
    * and BranchN.
    *
    * @param rootActions the number of actions available from the root node
    * @param m monoid over the observation implementation
    * @tparam O observations type
    * @return a tree root instance
    */
  def createRoot[O <: Observation](rootActions: Int)(implicit m: Monoid[O]): NewTree = {
    new NewTree.BranchN(BitSet(), m.empty, Array.fill(rootActions)(None))
  }

  /**
    * models a tree leaf, containing it's state and reward data
    * @param state the leaf's state
    * @param observations the leaf's reward data
    */
  final class Leaf(
    val state: MultiChoiceState,
    var observations: Observation
  ) extends NewTree

//  /**
//    * models a branch and it's singular leaf, a specialized case of a branch with 1 child
//    * @param state the branch's (partial) state
//    * @param observations reward data for this branch
//    * @param value value function evaluation for this node from its set of observed rewards
//    */
  final class Branch1(
    val state: MultiChoiceState,
    var observations: Observation,
    val childAction: Int,
    var child: NewTree
  ) extends NewTree

  /**
    * models a branch with n children
    * @param state the branch's (partial) state
    * @param observations reward data for this branch
    * @param children the children trees, indexed by their action number
    */
  final class BranchN(
    val state: MultiChoiceState,
    var observations: Observation,
    var children: Array[Option[NewTree]],
  ) extends NewTree
}

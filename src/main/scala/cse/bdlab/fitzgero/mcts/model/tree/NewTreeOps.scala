package cse.bdlab.fitzgero.mcts.model.tree

import cse.bdlab.fitzgero.mcts.model.observation.Observation
import cse.bdlab.fitzgero.mcts.model.observation.ObservationOps._
import cse.bdlab.fitzgero.mcts.model.state.combinatorial.MultiChoiceState
import cse.bdlab.fitzgero.mcts.model.value.ValueFunction
import cse.bdlab.fitzgero.mcts.model.value.ValueFunctionOps._

object NewTreeOps {

  // currently hard-coded for multi choice problems. different interpreters
  // should be written if other types of states are supported.

  final case class PreviousNodeAndTransitionAction(node: NewTree, action: Int)

  implicit class NewTreeOpsInstance(newTree: NewTree) {

    def state: MultiChoiceState = newTree match {
      case leaf: NewTree.Leaf      => leaf.state
      case branch: NewTree.Branch1 => branch.state
      case n: NewTree.BranchN      => n.state
    }

    def observations: Observation = newTree match {
      case leaf: NewTree.Leaf      => leaf.observations
      case branch: NewTree.Branch1 => branch.observations
      case n: NewTree.BranchN      => n.observations
    }

    /**
      * observe a simulation result, updating the local observations
      * @param observation the observation to record
      */
    def updateObservations(observation: Double): Unit = {

      // update the local observation
      newTree match {
        case leaf: NewTree.Leaf =>
          leaf.observations = leaf.observations.update(observation)
        case branch: NewTree.Branch1 =>
          branch.observations = branch.observations.update(observation)
        case branchN: NewTree.BranchN =>
          branchN.observations = branchN.observations.update(observation)
      }
    }

    /**
      * since there are different tree node classes, when a node transitions between classes,
      * we need to modify the reference to the node in the parent.
      *
      * this method is called on the parent node to update the reference to this new child node object.
      *
      * @param newChildNodeObject the new NewTree object
      * @param childAction the action the former NewTree object was associated with
      */
    def updateReferenceToChildNode(newChildNodeObject: NewTree, childAction: Int): Unit = {
      newTree match {
        case _: NewTree.Leaf =>
          throw new IllegalArgumentException("leaf cannot have children nodes")
        case b1: NewTree.Branch1 =>
          if (b1.childAction == childAction) {
            b1.child = newChildNodeObject
          } else {
            throw new IllegalArgumentException(
              s"updating reference to child node $childAction which didn't exist"
            )
          }
        case n: NewTree.BranchN =>
          try {
            n.children(childAction) match {
              case Some(_) =>
                n.children.update(childAction, Some(newChildNodeObject))
              case None =>
                throw new IllegalArgumentException(
                  s"updating reference to child node $childAction which didn't exist"
                )
            }
          } catch {
            case e: ArrayIndexOutOfBoundsException =>
              throw new IllegalArgumentException(
                "updating reference to child node which has an invalid action id",
                e
              )
          }
      }
    }

    /**
      * calculates the value entry for the given node, using the value function,
      * and providing the parent node, if any exists.
      *
      * @param parent the parent node, or None if the child is the tree root
      * @param vfn the function used to update values based on observations
      */
    def value(parent: Option[NewTree], vfn: ValueFunction[MultiChoiceState]): Double = {
      vfn.calculateValue(parent.map { _.observations }, newTree.observations)
    }

    /**
      * adds a child to this NewTree instance in-place as a mutable operation. assumed to
      * occur during the TreePolicy phase of MCTS and should not affect the
      *
      * @param parentOption the parent of this node, if it exists, along with
      *                     the action which was used to transition to this node
      * @param newChild the child to add
      * @param childAction the action used to result in this child
      * @param actions number of actions possible from this state
      */
    def addChild(
      parentOption: Option[PreviousNodeAndTransitionAction],
      newChild: NewTree.Leaf,
      childAction: Int,
      actions: Int
    ): NewTree = {
      newTree match {
        case leaf: NewTree.Leaf =>
          // transition Leaf into Branch1 by adding child and updating parent reference
          val b1 = new NewTree.Branch1(
            state = leaf.state,
            observations = leaf.observations,
            childAction = childAction,
            child = newChild
          )
          parentOption.foreach { pnta => pnta.node.updateReferenceToChildNode(b1, pnta.action) }
          b1

        case b1: NewTree.Branch1 =>
          // transition Branch1 to BranchN by adding child and updating parent reference
          val children: Array[Option[NewTree]] = Array
            .fill(actions)(None)
            .updated(b1.childAction, Some(b1.child))
            .updated(childAction, Some(newChild))

          val bn = new NewTree.BranchN(b1.state, b1.observations, children)
          parentOption.foreach { pnta => pnta.node.updateReferenceToChildNode(bn, pnta.action) }
          bn

        case bn: NewTree.BranchN =>
          // no transition, just an add
          bn.children.update(childAction, Some(newChild))
          bn
      }
    }
  }
}

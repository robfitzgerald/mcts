package cse.bdlab.fitzgero.mcts.model.tree

import scala.collection.BitSet

import cse.bdlab.fitzgero.mcts.TestTemplate
import cse.bdlab.fitzgero.mcts.model.observation.Observation
import cse.bdlab.fitzgero.mcts.model.observation.ObservationOps._
import cse.bdlab.fitzgero.mcts.model.state.combinatorial.MultiChoiceState
import cse.bdlab.fitzgero.mcts.model.tree.NewTreeOps._

class NewTreeOpsTest extends TestTemplate {
  "NewTreeOps" when {
    "addChild" when {
      "called on a leaf" should {
        "turn the leaf into a Branch1" in {
          val maxActions = 2
          val parentLeaf: NewTree = new NewTree.Leaf(BitSet.empty, Observation.Empty)
          val childAction = 1
          val childLeaf: NewTree.Leaf = new NewTree.Leaf(BitSet(1), Observation.Empty)
          val parentBranch1: NewTree = parentLeaf.addChild(None, childLeaf, childAction, maxActions)
          parentBranch1 match {
            case branch: NewTree.Branch1 =>
              branch.state should equal(parentLeaf.state)
              branch.observations should equal(parentLeaf.observations)
              branch.childAction should equal(childAction)
              branch.child should equal (childLeaf)
            case _ => fail("adding a child to a Leaf should turn it into a Branch1")
          }
        }
      }
      "called on a Branch1" should {
        "turn the Branch1 into a BranchN" in {
          val maxActions = 4
          val parentState: MultiChoiceState = BitSet.empty
          val parentObs = new Observation.AccumulatedMean(10.0, 0, 10.0, 10)
          val child1State = BitSet.empty
          val child1Action = 1
          val child1Obs = new Observation.AccumulatedMean(1.0, 0, 1, 1)
          val child1 = new NewTree.Leaf(child1State, child1Obs)
          val child2State = BitSet.empty
          val child2 = new NewTree.Leaf(child2State, Observation.Empty)
          val child2Action = 2
          val parentBranch1: NewTree = new NewTree.Branch1(parentState, parentObs, child1Action, child1)
          val parentBranchN: NewTree = parentBranch1.addChild(None, child2, child2Action, maxActions)

          parentBranchN.state should equal (parentState)
          parentBranchN.observations should equal(parentObs)
          parentBranchN match {
            case n: NewTree.BranchN =>
              n.children(0) should be (None)
              n.children(child1Action) match {
                case Some(leaf) =>
                  leaf match {
                    case leaf: NewTree.Leaf =>
                      leaf.state should equal(child1State)
                      leaf.observations should equal(child1Obs)
                    case other => fail(s"child 1 should be a leaf but found $other")
                  }
                case None => fail("child 1 had action #1 but is missing")
              }
              n.children(child2Action) match {
                case Some(leaf) =>
                  leaf match {
                    case leaf: NewTree.Leaf =>
                      leaf.state should equal(child2State)
                      // no observations for new child
                      leaf.observations.visits should equal(0)
                    case other => fail(s"child 2 should be a leaf but found $other")
                  }
                case None => fail("child 2 had action #2 but is missing")
              }
              n.children(3) should be (None)
            case _ =>
              fail("should have been converted to a BranchN representation")
          }
        }
      }
      "called on a BranchN" should {
        "add a child to the BranchN" in {

        }
      }
    }
  }
}

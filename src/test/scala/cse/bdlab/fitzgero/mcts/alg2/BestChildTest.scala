package cse.bdlab.fitzgero.mcts.alg2

import scala.collection.BitSet

import cse.bdlab.fitzgero.mcts.TestTemplate
import cse.bdlab.fitzgero.mcts.model.observation.Observation
import cse.bdlab.fitzgero.mcts.model.state.combinatorial.MultiChoice
import cse.bdlab.fitzgero.mcts.model.state.combinatorial.MultiChoiceStateOps._
import cse.bdlab.fitzgero.mcts.model.tree.NewTree
import cse.bdlab.fitzgero.mcts.model.value.ValueFunction

class BestChildTest extends TestTemplate {

  "BestChildTest" when {
    "run" when {
      "called on a node with no children" should {
        "return None" in {
          val rootState = BitSet()
          val leaf = new NewTree.Leaf(rootState, new Observation.AccumulatedMean())
          val vfn = new ValueFunction.PedrosoRei()
          BestChild.run(leaf, vfn) match {
            case Some(_) =>
              fail("expected None but found a node instead")
            case None =>
              succeed
          }
        }
      }
      "called on a node whos children have not been visited and have the same value" should {
        "choose one arbitrarily" in {
          val problem = Array(5,10,15)
          val mc = MultiChoice(problem)
          val rootState = BitSet()

          val firstChildState = BitSet().addChoiceToState(mc.statePartitionOffsets, 0, 1)
          val firstChild = new NewTree.Leaf(firstChildState, new Observation.AccumulatedMean())
          val secondChildState = BitSet().addChoiceToState(mc.statePartitionOffsets, 0, 2)
          val secondChild = new NewTree.Leaf(secondChildState,  new Observation.AccumulatedMean())
          val rootTree = new NewTree.BranchN(rootState, new Observation.AccumulatedMean(), Array(None, Some(firstChild), Some(secondChild)))
          val vfn = new ValueFunction.PedrosoRei()

          BestChild.run(rootTree, vfn) match {
            case None => fail("expected a best child")
            case Some(bestChild) =>
              bestChild should not equal rootTree
          }

        }
      }
      "called on a node whos children have different value" should {
        "choose the best child" in {
          val problem = Array(5,10,15)
          val mc = MultiChoice(problem)
          val rootState = BitSet()

          val firstChildState = BitSet().addChoiceToState(mc.statePartitionOffsets, 0, 1)
          val firstChild = new NewTree.Leaf(firstChildState, new Observation.AccumulatedMean())
          val secondChildState = BitSet().addChoiceToState(mc.statePartitionOffsets, 0, 2)
          val secondChild = new NewTree.Leaf(secondChildState,  new Observation.AccumulatedMean())
          val rootTree = new NewTree.BranchN(rootState, new Observation.AccumulatedMean(), Array(None, Some(firstChild), Some(secondChild)))
          val vfn = new ValueFunction.PedrosoRei()

          val firstObservation = 10.0
          val secondObservation = 20.0
          Backup(List(rootTree, firstChild), firstObservation, firstChildState, vfn)
          Backup(List(rootTree, secondChild), secondObservation, secondChildState, vfn)

          BestChild.run(rootTree, vfn) match {
            case None => fail("expected a best child")
            case Some(bestChild) =>
              bestChild.node should equal(firstChild)
          }
        }
      }
    }
  }
}

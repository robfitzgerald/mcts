package cse.bdlab.fitzgero.mcts.model.state.combinatorial

import scala.collection.BitSet
import cse.bdlab.fitzgero.mcts.model.state.combinatorial.MultiChoiceStateOps._

import cse.bdlab.fitzgero.mcts.TestTemplate

class MultiChoiceStateOpsTest extends TestTemplate {
  "MultiChoiceState" when {
    "addChoiceToState" when {
      "called on an empty BitSet to add an entry" should {
        "add it" in {
          val problem = Array(3,5,7)
          val mc = MultiChoice(problem)
          val emptyState = BitSet()

          // add choice "2" for set "1". given the MultiChoice above, that should correspond
          // with the second sub-problem which has 5 choices and begins at offset index 3
          val updated = emptyState.addChoiceToState(mc.statePartitionOffsets, 1, 2)
          updated.contains(5) should be (true)
        }
      }
      "called on an nonempty BitSet to add an entry" should {
        "add it and not lose the previous" in {
          val problem = Array(3,5,7)
          val mc = MultiChoice(problem)
          val choiceSet3Choice5 = BitSet(12)
          val updated =
            choiceSet3Choice5
              .addChoiceToState(mc.statePartitionOffsets, 1, 1)
              .addChoiceToState(mc.statePartitionOffsets, 2, 7)
          updated.contains(4) should be (true)
          updated.contains(12) should be (true)
        }
      }
    }
    "getChoice" when {
      "called on an empty BitSet" should {
        "result in None" in {
          val problem = Array(3,5,7)
          val mc = MultiChoice(problem)
          val empty = BitSet()
          empty.getChoice(mc, 1) should equal (None)
        }
      }
      "called on a BitSet with a valid entry" should {
        "provide that result" in {
          val problem = Array(3,5,7)
          val mc = MultiChoice(problem)
          val choiceSet1Choice1 = BitSet().addChoiceToState(mc.statePartitionOffsets, 1, 3)
          choiceSet1Choice1.getChoice(mc, 1) should equal (Some(3))
        }
      }
    }
  }
}

package cse.bdlab.fitzgero.mcts.model.state.combinatorial

import scala.collection.BitSet

import cse.bdlab.fitzgero.mcts.TestTemplate

class MultiChoiceTest extends TestTemplate {

  "MultiChoice" when {
    "apply" when {
      "called with an empty array" should {
        "create an empty MultiChoice problem" in {
          val mc = MultiChoice(Array.empty)
          mc.statePartitionOffsets should be (Array.empty)
          mc.statePartitionMasks should be (Array.empty)
        }
      }
      "called with an array with values" should {
        "create the associated MultiChoice problem" in {
          val problem = Array(3,5,7)
          val expectedOffsets = problem.scan(0)(_+_).take(3)
          val mc = MultiChoice(problem)

          mc.statePartitionOffsets should equal(expectedOffsets)

          mc.statePartitionMasks(0).intersect(Set(2)) should equal (BitSet(2))
          mc.statePartitionMasks(0).intersect(Set(4)) should equal (BitSet.empty)
          mc.statePartitionMasks(1).intersect(Set(5)) should equal (BitSet(5))
          mc.statePartitionMasks(1).intersect(Set(9)) should equal (BitSet.empty)

          val allMasksCombined = mc.statePartitionMasks(0).union(mc.statePartitionMasks(1)).union(mc.statePartitionMasks(2))
          allMasksCombined should equal (BitSet(0 until 15: _*))
        }
      }
    }
  }

}

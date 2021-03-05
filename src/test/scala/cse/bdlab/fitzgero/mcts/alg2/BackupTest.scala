package cse.bdlab.fitzgero.mcts.alg2

import scala.collection.BitSet

import cse.bdlab.fitzgero.mcts.TestTemplate
import cse.bdlab.fitzgero.mcts.model.state.combinatorial.MultiChoice
import cse.bdlab.fitzgero.mcts.model.state.combinatorial.MultiChoiceStateOps._
import cse.bdlab.fitzgero.mcts.model.tree.NewTree
import cse.bdlab.fitzgero.mcts.model.value.ValueFunction
import cse.bdlab.fitzgero.mcts.model.observation.Observation
import cse.bdlab.fitzgero.mcts.model.observation.ObservationOps._

class BackupTest extends TestTemplate {

  "BackupTest" when {
    "apply" when {
      "called on an empty path" should {
        "fail" in {
          val vfn = new ValueFunction.PedrosoRei()
           an [IllegalArgumentException] should be thrownBy { Backup(List.empty, 123.345, BitSet(), vfn) }
        }
      }
      "called on a singleton tree path (root)" should {
        "update the observations and values of the root" in {
          val rootState = BitSet()

          val rootTree = new NewTree.Leaf(rootState, new Observation.AccumulatedMean())

          val observation = 123.345
          val observedState = BitSet(1)
          val vfn = new ValueFunction.PedrosoRei()
          Backup(List(rootTree), observation, observedState, vfn)

          rootTree.observations.visits should equal (1)
          rootTree.observations.max should equal(observation)
          rootTree.observations.mean should equal(observation)
          rootTree.observations.min should equal(observation)

          vfn.globalMinObservation should equal(observation)
          vfn.globalMaxObservation should equal(observation)
          vfn.globalMinState should equal(observedState)
        }
      }
      "called on a path of two" should {
        "update the observations and values of both nodes" in {
          val problem = Array(5,10,15)
          val mc = MultiChoice(problem)
          val rootState = BitSet()

          val firstChildState = BitSet().addChoiceToState(mc.statePartitionOffsets, 0, 1)
          val firstChild = new NewTree.Leaf(firstChildState, Observation.Empty)
          val secondChildState = BitSet().addChoiceToState(mc.statePartitionOffsets, 0, 2)
          val secondChild = new NewTree.Leaf(secondChildState,  Observation.Empty)
          val rootTree = new NewTree.BranchN(rootState, Observation.Empty, Array(None, Some(firstChild), Some(secondChild)))

          val firstObservation = 10.0
          val secondObservation = 20.0
          val vfn = new ValueFunction.PedrosoRei()
          Backup(List(rootTree, firstChild), firstObservation, firstChildState, vfn)
          Backup(List(rootTree, secondChild), secondObservation, secondChildState, vfn)

          // confirm all observations are correct
          vfn.globalMinObservation should equal (10.0)
          vfn.globalMaxObservation should equal (20.0)
          vfn.globalMinState should equal(firstChildState)

          rootTree.observations match {
            case obs: Observation.AccumulatedMean =>
              obs.observationsSum should equal (30.0)
              obs.observedMin should equal (10.0)
              obs.observedMax should equal (20.0)
              obs.count should equal (2)
            case _ => fail("should have an accumulated mean observation representation")
          }

          firstChild.observations match {
            case obs: Observation.Single =>
              obs.observation should equal (firstObservation)
            case _ => fail("should have a Single observation representation")
          }

          secondChild.observations match {
            case obs: Observation.Single =>
              obs.observation should equal (secondObservation)
            case _ => fail("should have a Single observation representation")
          }
        }
      }
    }
  }
}

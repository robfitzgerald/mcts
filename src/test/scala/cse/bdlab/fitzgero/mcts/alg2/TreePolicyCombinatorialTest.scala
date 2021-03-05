package cse.bdlab.fitzgero.mcts.alg2

import scala.collection.BitSet

import cats.Monoid

import cse.bdlab.fitzgero.mcts.TestTemplate
import cse.bdlab.fitzgero.mcts.model.observation.{Observations, ObservationsOps}
import cse.bdlab.fitzgero.mcts.model.observation.ObservationsOps._
import cse.bdlab.fitzgero.mcts.model.state.combinatorial.MultiChoice
import cse.bdlab.fitzgero.mcts.model.state.combinatorial.MultiChoiceStateOps._
import cse.bdlab.fitzgero.mcts.model.tree.NewTree
import cse.bdlab.fitzgero.mcts.model.tree.NewTreeOps._
import cse.bdlab.fitzgero.mcts.model.value.ValueFunction

class TreePolicyCombinatorialTest extends TestTemplate {

  "UCTTreePolicyCombinatorialTest" when {
    "run" when {
      "called on a tree with only a root" should {
        "return a path which includes only the root" in {
          val mc                                   = MultiChoice(Array.empty)
          val root                                 = NewTree.createRoot(0)
          val vfn                                  = new ValueFunction.PedrosoRei()
          val sFn: (Int, Set[Int]) => Int          = (_: Int, as: Set[Int]) => as.head
          val treePolicy                           = TreePolicyCombinatorial(mc, sFn)
          val TreePolicyResult(path, selectedNode, depth) = treePolicy.run(root, vfn)
          path.length should equal(1)
          path.head should equal(root)
          selectedNode should equal (root)
        }
      }
      "called on a search tree which is full" should {
        "return a path which includes the root and the higher-value child" in {
          val m: Monoid[Observations.ObservationsDoublePrecision] = ObservationsOps.ObsDoubleMonoid
          val actions                                             = 2
          val problem: Array[Int]                                 = Array(actions)
          val mc                                                  = MultiChoice(problem)

          val firstChildAction = 0
          val firstChildState  = BitSet().addChoiceToState(mc.statePartitionOffsets, 0, 0)
          val firstChild       = new NewTree.Leaf(firstChildState, m.empty)

          val secondChildAction = 1
          val secondChildState  = BitSet().addChoiceToState(mc.statePartitionOffsets, 0, 1)
          val secondChild       = new NewTree.Leaf(secondChildState, m.empty)

          val rootTree: NewTree = NewTree.createRoot(actions)
          rootTree
            .addChild(None, firstChild, firstChildAction, actions)
            .addChild(None, secondChild, secondChildAction, actions)

          val vfn               = new ValueFunction.PedrosoRei()
          val firstObservation  = 10.0
          val secondObservation = 20.0
          Backup(List(rootTree, firstChild), firstObservation, firstChildState, vfn)
          Backup(List(rootTree, secondChild), secondObservation, secondChildState, vfn)

          val sFn: (Int, Set[Int]) => Int = (_: Int, _: Set[Int]) => -1
          val treePolicy                  = TreePolicyCombinatorial(mc, sFn)
          val TreePolicyResult(path, selectedNode, depth) = treePolicy.run(rootTree, vfn)

          path.length should equal(2)
          path.head should equal(rootTree)
          path.last should equal(firstChild)
          selectedNode should equal (firstChild)
        }
      }
      "called on a search tree which can be expanded from a leaf (vertical)" should {
        "return a path which includes the root, the higher-value root child, and a new node" in {
          val m: Monoid[Observations.ObservationsDoublePrecision] = ObservationsOps.ObsDoubleMonoid
          val problem: Array[Int]                                 = Array(2, 1)
          val mc: MultiChoice                                     = MultiChoice.apply(actions = problem)

          val firstChildAction = 0
          val firstChildState  = BitSet().addChoiceToState(mc.statePartitionOffsets, 0, 0)
          val firstChild       = new NewTree.Leaf(firstChildState, m.empty)

          val secondChildAction = 1
          val secondChildState  = BitSet().addChoiceToState(mc.statePartitionOffsets, 0, 1)
          val secondChild       = new NewTree.Leaf(secondChildState, m.empty)

          var rootTree: NewTree = NewTree.createRoot(2)
          rootTree = rootTree
            .addChild(None, firstChild, firstChildAction, 2)
            .addChild(None, secondChild, secondChildAction, 2)

          val vfn               = new ValueFunction.PedrosoRei()
          val firstObservation  = 10.0
          val secondObservation = 20.0
          Backup(List(rootTree, firstChild), firstObservation, firstChildState, vfn)
          Backup(List(rootTree, secondChild), secondObservation, secondChildState, vfn)

          val sFn: (Int, Set[Int]) => Int = (_: Int, alts: Set[Int]) => alts.head
          val treePolicy                  = TreePolicyCombinatorial(mc, sFn)
          val TreePolicyResult(path, selectedNode, depth) = treePolicy.run(rootTree, vfn)

          path.length should equal(3)
          path.head should equal(rootTree)
          path.tail.head.observations should equal(firstChild.observations)
          path.tail.head match {
            case _: NewTree.Leaf    => fail("should now be a Branch1")
            case _: NewTree.BranchN => fail("should be a Branch1")
            case b1: NewTree.Branch1 =>
              b1.childAction should equal(0) // took action 0 to reach new child
              b1.child shouldBe a[NewTree.Leaf]
          }
          selectedNode should equal(path.tail.tail.head)
        }
      }
      "called on a tree with unexplored children" should {
        "return a path which includes the root and a newly-instantiated child" in {
          val m: Monoid[Observations.ObservationsDoublePrecision] = ObservationsOps.ObsDoubleMonoid
          val actions                                             = 5
          val problem: Array[Int]                                 = Array(actions)
          val mc                                                  = MultiChoice(problem)

          val firstChildAction = 1
          val firstChildState  = BitSet().addChoiceToState(mc.statePartitionOffsets, 0, 1)
          val firstChild       = new NewTree.Leaf(firstChildState, m.empty)

          val secondChildAction = 2
          val secondChildState  = BitSet().addChoiceToState(mc.statePartitionOffsets, 0, 2)
          val secondChild       = new NewTree.Leaf(secondChildState, m.empty)

          var rootTree: NewTree = NewTree.createRoot(actions)
          rootTree = rootTree
            .addChild(None, firstChild, firstChildAction, actions)
            .addChild(None, secondChild, secondChildAction, actions)

          val vfn               = new ValueFunction.PedrosoRei()
          val firstObservation  = 10.0
          val secondObservation = 20.0
          Backup(List(rootTree, firstChild), firstObservation, firstChildState, vfn)
          Backup(List(rootTree, secondChild), secondObservation, secondChildState, vfn)

          val childToPick                 = 3
          val sFn: (Int, Set[Int]) => Int = (_: Int, _: Set[Int]) => childToPick
          val treePolicy                  = TreePolicyCombinatorial(mc, sFn)
          val TreePolicyResult(path, selectedNode, depth) = treePolicy.run(rootTree, vfn)

          val thirdChild = rootTree match {
            case n: NewTree.BranchN =>
              n.children(childToPick).getOrElse(throw new Error("invariant failed"))
            case other => fail(s"unexpected root tree not BranchN type: $other")
          }

          path.length should equal(2)
          path.head should equal(rootTree)
          path.last should equal(thirdChild)
          selectedNode should equal (thirdChild)
        }
      }
    }
  }
}

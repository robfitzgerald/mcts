package cse.bdlab.fitzgero.mcts

import scala.util.Random

import cats.Monoid

import cse.bdlab.fitzgero.mcts.alg2.TreePolicyCombinatorial
import cse.bdlab.fitzgero.mcts.model.observation.Observation
import cse.bdlab.fitzgero.mcts.model.observation.ObservationOps
import cse.bdlab.fitzgero.mcts.model.state.combinatorial.MultiChoiceStateOps._
import cse.bdlab.fitzgero.mcts.model.state.combinatorial.{MultiChoice, MultiChoiceState}
import cse.bdlab.fitzgero.mcts.model.tree.NewTree
import cse.bdlab.fitzgero.mcts.model.tree.NewTreeOps._
import cse.bdlab.fitzgero.mcts.model.value.ValueFunction
import cse.bdlab.fitzgero.mcts.model.value.ValueFunction.PedrosoRei

class MCTS2Test extends TestTemplate {

  "MCTS2Test" when {
    "uctSearch" when {
      "called with a simple combinatorial problem" should {
        "search the problem for an optimum" in {
          implicit val m: Monoid[Observation] = ObservationOps.ObservationMonoid

          val random = Random

          val slots = 10
          val actions       = Array.fill(slots)(10) // 10^5 = 100k combinations
          val maxIterations = 500000
          val multiChoice   = MultiChoice(actions)
          // costs are equal to the slot plus the action index
          val costsTable = for {
            slot <- (0 until slots).toArray
          } yield for {
            action <- (0 until 10).toArray
          } yield {
            (slot + action).toDouble
          }

          def costFn(multiChoiceState: MultiChoiceState): Double = {
            val costs = for {
              (actionOption, slot) <- multiChoiceState.getChoices(multiChoice).zipWithIndex
              actionChosen <- actionOption
              cost = costsTable(slot)(actionChosen)
            } yield (slot, actionChosen, cost)
            if (costs.isEmpty) 0.0
            else costs.map { case (_, _, cost) => cost }.sum
          }

          def defaultPolicy(multiChoiceState: MultiChoiceState): MultiChoiceState = {
            multiChoiceState.getChoices(multiChoice).zipWithIndex.foldLeft(multiChoiceState) { case (mcs, (actionOption, slot)) =>
              actionOption match {
                case Some(_) => mcs
                case None =>
                  val selection = random.nextInt(10)
                  mcs.addChoiceToState(multiChoice.statePartitionOffsets, slot, selection)
              }
            }
          }

          val explorationFunction  = new ValueFunction.PedrosoRei()

          def expansionFunction(int: Int, validChoices: Set[Int]): Int = {
            val lookup = validChoices.toArray
            lookup(random.nextInt(lookup.length))
          }

          def computeBudgetFunction(root: NewTree, iterations: Int, startTime: Long): Boolean = {
            iterations < maxIterations
          }

          val result = MCTS2.uctSearch(
            actions,
            costFn,
            defaultPolicy,
            explorationFunction,
            expansionFunction,
            computeBudgetFunction,
            computeBudgetTestRate = 100
          )

          val exploitationFunction = new PedrosoRei(
            Cp = 0.0,
            globalMinObservation = explorationFunction.globalMinObservation,
            globalMaxObservation = explorationFunction.globalMaxObservation,
          )

          result match {
            case Left(value) => fail(value)
            case Right(value) =>
              value match {
                case root: NewTree.BranchN =>
                  val bestResult =
                    TreePolicyCombinatorial(multiChoice, expansionFunction)
                      .run(root, exploitationFunction)
                  bestResult.path.foreach(println)
                  println(f"best node state               : ${bestResult.selectedNode.state}")
                  val parent = bestResult.path.reverse.tail.headOption
                  println(f"best node value (exploration) : ${bestResult.selectedNode.value(parent, explorationFunction)}%.2f")
                  println(f"best node value (exploitation): ${bestResult.selectedNode.value(parent, exploitationFunction)}%.2f")

                  val hist:Array[Int] = Array.fill(slots + 1)(0)
                  def nodeHistogram(tree: NewTree): Unit = {
                    tree match {
                      case _: NewTree.Leaf => hist.update(0, hist(0) + 1)
                      case branch: NewTree.Branch1 =>
                        nodeHistogram(branch.child)
                        hist.update(1, hist(1) + 1)
                      case n: NewTree.BranchN =>
                        val children = n.children.flatten
                        children.foreach { nodeHistogram }
                        val nChildren = children.length
                        hist.update(nChildren, hist(nChildren) + 1)
                    }
                  }
                  nodeHistogram(root)
                  val histogramString = hist
                    .zipWithIndex
                    .map{ case (cnt, idx) => f"  children=$idx: count=$cnt"}
                    .mkString("histogram of children count\n", "\n", "")
                  println(histogramString)
                case other =>
                  fail(s"invalid root representation: $other")
              }
          }
        }
      }
    }
  }
}

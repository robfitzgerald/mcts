package cse.bdlab.fitzgero.mcts

import cats.Monoid

import cse.bdlab.fitzgero.mcts.alg2.{Backup, TreePolicyCombinatorial, TreePolicyResult}
import cse.bdlab.fitzgero.mcts.model.observation.Observation
import cse.bdlab.fitzgero.mcts.model.state.combinatorial.{MultiChoice, MultiChoiceState}
import cse.bdlab.fitzgero.mcts.model.state.combinatorial.MultiChoiceStateOps._
import cse.bdlab.fitzgero.mcts.model.tree.NewTree
import cse.bdlab.fitzgero.mcts.model.tree.NewTreeOps._
import cse.bdlab.fitzgero.mcts.model.value.ValueFunction
import cse.bdlab.fitzgero.mcts.model.value.ValueFunctionOps._

object MCTS2 {

  /**
    * run a UCT MCTS search with the given arguments.
    *
    * @param actions for each slot, the number of actions that can be selected
    * @param evaluation evaluates the cost function at a state
    * @param defaultPolicy simulates a terminal state from some selected state
    * @param valueFunction calculates the value function for a tree node based on the observations of the tree
    * @param expandSelectionFunction chooses a child to expand using the slot #
    *                                and the set of valid (unexplored) actions at that slot
    * @param computeBudgetFunction determines if computation can continue (true) or should terminate (false)
    * @param computeBudgetTestRate frequency to test compute budget, in iterations
    * @param m monoid instance for Observation objects, providing "empty" and "combine" methods
    * @tparam O observation type
    * @return either the search tree, or, an error
    */
  def uctSearch[O <: Observation](
    actions                : Array[Int],
    evaluation             : MultiChoiceState => Double,
    defaultPolicy          : MultiChoiceState => MultiChoiceState,
    valueFunction          : ValueFunction[MultiChoiceState],
    expandSelectionFunction: (Int, Set[Int]) => Int,
    computeBudgetFunction  : (NewTree, Int, Long) => Boolean,
    computeBudgetTestRate  : Int = 1
  )(implicit m: Monoid[O]): Either[Error, NewTree] = {
    val result = for {
      _ <- validateInput(actions, computeBudgetTestRate)
      startTime   = System.currentTimeMillis
      multiChoice = MultiChoice(actions)
      rootActions = actions(0)
      root        = NewTree.createRoot(rootActions)
      treePolicy  = TreePolicyCombinatorial(multiChoice, expandSelectionFunction)
    } yield {

      // begin MCTS
      var withinComputeBudget = true
      var iterations          = 0
      do {
        // run search
        val TreePolicyResult(path, selectedNode, depth) = treePolicy.run(root, valueFunction)
        val simulatedState: MultiChoiceState            = defaultPolicy(selectedNode.state)
        val observation: Double                         = evaluation(simulatedState)
        Backup(path, observation, simulatedState, valueFunction)

        // update search state
        val callTerminationFn = computeBudgetTestRate == 1 || iterations % computeBudgetTestRate == 0
        if (callTerminationFn) {
          withinComputeBudget = computeBudgetFunction(root, iterations, startTime)
        }
        iterations += 1

      } while (withinComputeBudget)

      root
    }
    result
  }

  /**
    * check that the user-provided inputs are correct (where possible)
    * @param actions represents the multi-set indices of this problem
    * @param testRate frequency we test for termination
    * @return (), or, an error
    */
  def validateInput(actions: Array[Int], testRate: Int): Either[Error, Unit] = {
    if (testRate < 1) {
      Left(new Error(s"terminationTestRate must be positive, found $testRate"))
    } else if (actions.lengthCompare(1) < 0) {
      Left(new Error(s"action space must not be empty"))
    } else {
      Right(())
    }
  }
}

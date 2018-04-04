package cse.fitzgero.mcts.example.mckdv

import java.time.Instant

import cse.fitzgero.mcts.core._
import cse.fitzgero.mcts.example.mckdv.MCKDV.{Selection, Choice, Dependency, costOfSelection}
import cse.fitzgero.mcts.tree.MCTreeStandardReward
import cse.fitzgero.mcts.variant.StandardMCTS
/**
  * Mutliple Choice Knapsack Problems with Dependent Weights
  */
class MCKDVSolver (problem: Set[Map[Choice, List[Dependency]]], seed: Long = 0L, timeBudget: Long = 5000L) extends StandardMCTS[Selection, Choice] {

  // Selection : Map[Choice, List[Dependency]]

  override def applyAction(state: Selection, action: Choice): Selection = {
    val choices: Map[Choice, List[Dependency]] = problem.flatten.toMap
    state + (action -> choices(action))
  }

  override def evaluateTerminal(state: Selection): Double = costOfSelection(state)

  override def getSearchCoefficients(tree: MCTreeStandardReward[Selection, Choice]): Coefficients = ExplorationCoefficient

  override def getDecisionCoefficients(tree: MCTreeStandardReward[Selection, Choice]): Coefficients = SearchCoefficient

  override def generatePossibleActions(state: Selection): Seq[Choice] = {
    val remainingSubsets = for {
      subset <- problem
      if subset.keys.toSet.intersect(state.keys.toSet).isEmpty
    } yield subset

    remainingSubsets.flatten.map{_._1}.toSeq
  }

  override def selectAction(actions: Seq[Choice]): Option[Choice] = actionSelection.selectAction(actions)

  override def stateIsNonTerminal(state: Selection): Boolean = state.size != problem.size

  override def startState: Selection = Map()

  override def random: RandomGenerator = new BuiltInRandomGenerator(Some(seed))

  override val terminationCriterion = TimeTermination(Instant.now, timeBudget)
  override val actionSelection = RandomSelection(random, generatePossibleActions)
}

object MCKDVSolver {
  def apply(problem: Set[Map[Choice, List[Dependency]]]): MCKDVSolver = new MCKDVSolver(problem)
}
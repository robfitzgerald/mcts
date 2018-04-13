package cse.fitzgero.mcts.example.mckdv.standardmcts

import cse.fitzgero.mcts.core.terminationcriterion.{IterationTermination, TerminationCriterion02}
import cse.fitzgero.mcts.core.{BuiltInRandomGenerator, RandomGenerator, RandomSelection}
import cse.fitzgero.mcts.example.mckdv.implementation.MCKDV._
import cse.fitzgero.mcts.variant.StandardMCTS

trait MCKDVStandardMCTS extends StandardMCTS[Selection, Choice] {
  def problem: Problem
  def costBound: Int
  def seed: Long
  def timeBudget: Long

  // user must implement evaluateTerminal, getSearchCoefficients, and getDecisionCoefficients

  final override def applyAction(state: Selection, action: Choice): Selection = state + action
  final override def generatePossibleActions(state: Selection): Seq[Choice] = {
    val remainingSubsets = for {
      subset <- problem.multiset
      if subset.intersect(state).isEmpty
    } yield subset

    remainingSubsets.flatten.toSeq
  }
  final override def selectAction(actions: Seq[Choice]): Option[Choice] = actionSelection.selectAction(actions)
  final override def stateIsNonTerminal(state: Selection): Boolean = state.size != problem.multiset.size
  final override def startState: Selection = Set()
  final override def random: RandomGenerator = new BuiltInRandomGenerator(Some(seed))
//  final override val terminationCriterion: TimeTermination02[Selection,Choice,Tree] = TimeTermination02[Selection,Choice,Tree](timeBudget)
  final override val terminationCriterion: TerminationCriterion02[Selection, Choice, Tree] = IterationTermination[Selection,Choice,Tree](timeBudget)
  final override val actionSelection = RandomSelection(random, generatePossibleActions)
}

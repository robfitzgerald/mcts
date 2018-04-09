package cse.fitzgero.mcts.example.mckdv

import java.time.Instant

import cse.fitzgero.mcts.core._
import cse.fitzgero.mcts.example.mckdv.MCKDV._
import cse.fitzgero.mcts.tree.MCTreeStandardReward
import cse.fitzgero.mcts.variant.StandardMCTS
/**
  * Mutliple Choice Knapsack Problems with Dependent Weights
  */
class MCKDVSolver (problem: Problem, costBound: Int, seed: Long = 0L, timeBudget: Long = 5000L) extends StandardMCTS[Selection, Choice] {

//  case class Dependency(cost: Int, dst: Choice) extends HasCost
//  case class Choice(label: Int, cost: Int) extends HasCost
//  type Dependencies = Map[Choice, List[Dependency]]
//  type Multiset = Set[Set[Choice]]
//  type Problem = (Multiset, Dependencies)
//  type Selection = Map[Choice, List[Dependency]]

  // costBound * 2 set as a value that could dominate over any Choice costs and Dependency costs.
  // see costBound * 2 also in MCKDVGenerator
  val maxPossibleCost: BigDecimal = problem.multiset.size * problem.multiset.head.size * costBound * 2

  override def applyAction(state: Selection, action: Choice): Selection = state + action

  override def evaluateTerminal(state: Selection): Double = (costOfSelection(state, problem.dependencies) / maxPossibleCost).toDouble

  override def getSearchCoefficients(tree: MCTreeStandardReward[Selection, Choice]): Coefficients = ExplorationCoefficient

  override def getDecisionCoefficients(tree: MCTreeStandardReward[Selection, Choice]): Coefficients = SearchCoefficient

  override def generatePossibleActions(state: Selection): Seq[Choice] = {
    val remainingSubsets = for {
      subset <- problem.multiset
      if subset.intersect(state).isEmpty
    } yield subset

    remainingSubsets.flatten.toSeq
  }

  override def selectAction(actions: Seq[Choice]): Option[Choice] = actionSelection.selectAction(actions)

  override def stateIsNonTerminal(state: Selection): Boolean = state.size != problem.multiset.size

  override def startState: Selection = Set()

  override def random: RandomGenerator = new BuiltInRandomGenerator(Some(seed))

  override val terminationCriterion = TimeTermination(Instant.now, timeBudget)
  override val actionSelection = RandomSelection(random, generatePossibleActions)
}

object MCKDVSolver {
  def apply(problem: Problem, costBound: Int): MCKDVSolver = new MCKDVSolver(problem, costBound)
  def apply(problem: Problem, costBound: Int, seed: Long, timeBudget: Long): MCKDVSolver = new MCKDVSolver(problem, costBound, seed, timeBudget)
}
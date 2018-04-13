package cse.fitzgero.mcts.example.mckdv.implementation

//import org.scalacheck._
//import Gen._
//import Arbitrary.arbitrary

import cse.fitzgero.mcts.algorithm.samplingpolicy.banditfunction.UCT_PedrosoRei.{Maximize, Minimize, Objective}
import cse.fitzgero.mcts.example.mckdv.implementation.MCKDV._

/**
  * generates Multiple Choice Knapsack Problems with Dependent Values
  */
trait MCKDVGenerator {

  def random: scala.util.Random
  def costBound: Int

  def genProblem(n: Int, k: Int, objective: Objective = Maximize): (Problem, Set[Choice]) = {
    val mset = genMultiset(n,k)
    val deps = genDependencies(mset, objective)
    createOptimalCombination((mset,deps), objective)
  }

  def createOptimalCombination(problem: Problem, objective: Objective = Maximize): (Problem, Set[Choice]) = {
    val chosenOptimalCombination: Set[Choice] =
      problem
        .multiset
        .map {
          set =>
            set.toVector(random.nextInt(set.size))
        }
    val updatedDependencies = chosenOptimalCombination.foldLeft(problem.dependencies)(
      (acc, choice) => {
        val thisChoiceDeps = acc(choice)
        val updatedDeps = thisChoiceDeps.map {
          dep =>
            if (chosenOptimalCombination(dep.dst)) objective match {
              case Maximize =>
                Dependency(2 * costBound, dep.dst)
              case Minimize =>
                Dependency(0, dep.dst)
            }
            else dep
        }
        acc.updated(choice, updatedDeps)
      }
    )
    ((problem.multiset, updatedDependencies), chosenOptimalCombination)
  }

  def genMultiset(n: Int, k: Int): Multiset = {
    if (n < 1 || k < 1) {
      Set()
    } else {
      (0 until n).foldLeft(Set.empty[Set[Choice]]){
        (ms, nId) =>
          ms + (0 until k).map{kId => Choice((nId * k) + kId, random.nextInt(costBound))}.toSet
      }
    }
  }

  def genDependencies(multiset: Multiset, objective: Objective = Maximize): Dependencies =
    multiset.aggregate(Map.empty[Choice, List[Dependency]]) (
      (acc, set) => {
        acc ++ set.foldLeft(Map.empty[Choice, List[Dependency]]) {
          (ys, choice) => ys ++ createDependenciesForChoice(multiset, choice, objective)
        }
      }
    , (left, right) => {
        val overlap: Set[Choice] = left.keySet.intersect(right.keySet)
        val justLeft = left.filter(choiceWithDepts => !overlap(choiceWithDepts._1))
        val justRight = right.filter(choiceWithDepts => !overlap(choiceWithDepts._1))
        val overlapDeps = overlap.foldLeft(Map.empty[Choice, List[Dependency]])(
          (acc, choice) => {
            val leftDeps = left(choice)
            val rightDeps = right(choice)
            acc + (choice -> (leftDeps ::: rightDeps))
          }
        )
        justLeft ++ justRight ++ overlapDeps
      }
    )

  /**
    * generates all possible dependencies for a choice, setting all within a range that leaves space for some true optimal
    * @param set the problem
    * @param choice the choice we are creating dependencies for
    * @param objective influences the range where the values can be found, depending on where we should find the optimal (see createOptimalCombination())
    * @return
    */
  private def createDependenciesForChoice(set: Multiset, choice: Choice, objective: Objective): Dependencies = {
    val otherChoices: Set[Choice] = set.filter(set => !set(choice)).flatten
    val deps: List[Dependency] = otherChoices.map{
      otherChoice =>
        objective match {
          case Maximize =>
            val costBetweenZeroAndCostBound: Int = random.nextInt(costBound)
            Dependency(costBetweenZeroAndCostBound, otherChoice)
          case Minimize =>
            val costAboveCostBound: Int = random.nextInt(costBound) + (2 * costBound)
            Dependency(costAboveCostBound, otherChoice)
        }
    }.toList
    Map(choice -> deps)
  }
}

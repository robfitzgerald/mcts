package cse.fitzgero.mcts.example.mckdv

//import org.scalacheck._
//import Gen._
//import Arbitrary.arbitrary

import cse.fitzgero.mcts.example.mckdv.MCKDV._

trait MCKDVGenerator {

  def random: scala.util.Random
  def costBound: Int

  def genProblem(n: Int, k: Int): (Problem, Set[Choice]) = {
    val mset = genMultiset(n,k)
    val deps = genDependencies(mset)
    createOptimalCombination((mset,deps))
  }

  def createOptimalCombination(problem: Problem): (Problem, Set[Choice]) = {
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
            if (chosenOptimalCombination(dep.dst)) Dependency(2 * costBound, dep.dst)
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

  def genDependencies(multiset: Multiset): Dependencies =
    multiset.aggregate(Map.empty[Choice, List[Dependency]]) (
      (acc, set) => {
        acc ++ set.foldLeft(Map.empty[Choice, List[Dependency]]) {
          (ys, choice) => ys ++ createDependenciesForChoice(multiset, choice)
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

  private def createDependenciesForChoice(set: Multiset, choice: Choice): Dependencies = {
    val otherChoices: Set[Choice] = set.filter(set => !set(choice)).flatten
    val deps: List[Dependency] = otherChoices.map{
      otherChoice =>
        Dependency(random.nextInt(costBound), otherChoice)
    }.toList
    Map(choice -> deps)
  }
}

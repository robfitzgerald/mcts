package cse.bdlab.fitzgero.mcts.example.mckdv.implementation

import cse.bdlab.fitzgero.mcts.algorithm.samplingpolicy.banditfunction.UCT_PedrosoRei.Maximize
import cse.bdlab.fitzgero.mcts.example.mckdv.implementation.MCKDV._
import cse.bdlab.fitzgero.mcts.TestTemplate
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks

class MCKDVTests extends TestTemplate with PropertyChecks {
  "MCKDV" when {
    val reasonableValues: Gen[Int] = for ( i <- Gen.choose(1,1000)) yield i
    "costOfSelection" when {
      "given an empty selection" should {
        "produce a cost of zero" in {
          MCKDV.costOfSelection(Set(), Map()) should equal (0)
        }
      }
      "given a selection with one Choice" should {
        "produce the cost of that Choice" in {
          MCKDV.costOfSelection(Set(MCKDV.Choice(1,1)), Map(MCKDV.Choice(1,1) -> Nil)) should equal (1)
        }
      }
      "given a selection with two Choices and no dependencies" should {
        "produce the sum of costs for both Choices" in {
          val a = MCKDV.Choice(1,1)
          val b = MCKDV.Choice(1,10)
          val test: MCKDV.Dependencies = Map(a -> Nil,b -> Nil)
          MCKDV.costOfSelection(Set(a,b), test) should equal (a.cost + b.cost)
        }
      }
      "given a selection with two Choices and dependencies" should {
        "produce the sum of costs and dependencies for both Choices" in {
          val a = MCKDV.Choice(1,1)
          val b = MCKDV.Choice(1,10)
          val bDep = MCKDV.Dependency(4, a)
          val test: MCKDV.Dependencies = Map(a -> Nil,b -> List(bDep))
          MCKDV.costOfSelection(Set(a,b), test) should equal (MCKDV.costsOf(a :: b :: bDep :: Nil))
        }
      }
      "given a selection with two Choices and many dependencies" should {
        "produce the sum of costs and dependencies for both Choices" in {
          val a = MCKDV.Choice(1,1)
          val b = MCKDV.Choice(1,10)
          val aDep = List(MCKDV.Dependency(3, b), MCKDV.Dependency(12, b))
          val bDep = List(MCKDV.Dependency(4, a), MCKDV.Dependency(5, a), MCKDV.Dependency(10, a))
          val test: MCKDV.Dependencies = Map(a -> aDep,b -> bDep)
          MCKDV.costOfSelection(Set(a,b), test) should equal (MCKDV.costsOf(a :: b :: Nil ::: aDep ::: bDep))
        }
      }
    }
    "MCKDVGenerator" when {
      "genMultiset" when {
        "called with illegal values" should {
          "produce nothing" in new MCKDVGenerator {
            val random: scala.util.Random = scala.util.Random
            val costBound = 10
            genMultiset(-1,10) should equal (Set())
            genMultiset(10,-1) should equal (Set())
          }
        }
        "called with some normal n and k" should {
          "produce a Multiset of Choices" in new MCKDVGenerator {
            val random: scala.util.Random = scala.util.Random
            val costBound = 10
            forAll (reasonableValues, reasonableValues) {
              (n, k) =>
                val result: MCKDV.Multiset = genMultiset(n,k)
                result.size should equal (n)
                result.flatten.size should equal (n*k)
            }
          }
        }
      }
      "genDependencies" when {
        "called with an empty multiset" should {
          "create no dependencies" in new MCKDVGenerator {
            val random: scala.util.Random = scala.util.Random
            val costBound = 10
            genDependencies(Set(), Maximize()) should equal (Map())
          }
        }
        "called with a multiset with one entry" should {
          "create no dependencies" in new MCKDVGenerator {
            val random: scala.util.Random = scala.util.Random
            val costBound = 10
            val singleChoice = MCKDV.Choice(1, 10)
            genDependencies(Set(Set(singleChoice)), Maximize()) should equal (Map(singleChoice -> List()))
          }
        }
        "show me" should {
          "ok" in new MCKDVGenerator {
            val random: scala.util.Random = scala.util.Random
            val costBound = 10
            val multiset: Multiset = genMultiset(4, 4)
            val deps: Dependencies = genDependencies(multiset, Maximize())
            println("original")
            multiset.flatten.foreach{
              choice =>
                println(choice)
                println(deps(choice))
            }
            val (withOpt, optComb) = createOptimalCombination((multiset, deps), Maximize())
            println("optimal combination")
            println(optComb)
            println("with optimal")
            withOpt.multiset.flatten.foreach{
              choice =>
                println(choice)
                println(withOpt.dependencies(choice))
            }
          }
        }
      }
    }
  }
}

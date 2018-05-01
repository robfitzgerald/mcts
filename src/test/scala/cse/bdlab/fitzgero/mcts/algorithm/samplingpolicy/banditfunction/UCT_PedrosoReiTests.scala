package cse.bdlab.fitzgero.mcts.algorithm.samplingpolicy.banditfunction

import cse.bdlab.fitzgero.mcts.algorithm.samplingpolicy.banditfunction.UCT_PedrosoRei.{Maximize, Minimize}
import cse.bdlab.fitzgero.mcts.TestTemplate
import org.scalatest.prop.PropertyChecks

class UCT_PedrosoReiTests extends TestTemplate with PropertyChecks {
  "UCT_PedrosoRei" when {
    "uctExploration" when {
      "given the correct coefficients" should {
        "should have an inverse to 1.0" in {
          val inverse = 1D/math.sqrt(math.log(2)/2)
          val visits = 2
          UCT_PedrosoRei.uctExploration(inverse, visits, visits) should equal (1D)
        }
      }
      "given sqrt(2) as a Cp value" should {
        "give a child visited once a high reward" in {
          val Cp = math.sqrt(2)
          val parentVisits = 10
          val childVisits = 1
          UCT_PedrosoRei.uctExploration(Cp, parentVisits, childVisits) should be > 1D
        }
      }
    }
    "exploration" when {
      "child hasn't been visited" should {
        "produce the same value whether minimizing or maximizing" in {
          val min = UCT_PedrosoRei(100, 1000, 1000, 1000, 0, 1, math.sqrt(2))
          val max = UCT_PedrosoRei(1000, 100, 100, 100, 0, 1, math.sqrt(2))
          min should equal (Double.PositiveInfinity)
          min should equal (max)
        }
      }
      "exploitationTerm()" when {
        "global best and worst are the same" should {
          "return 0" in {
            UCT_PedrosoRei.pedrosoReiExploitationTerm(1000,1000,1000) should equal (Double.PositiveInfinity)
          }
        }
        "child best is equal to global best" should {
          "be equal to 1.0" in {
            val minimizeCase = UCT_PedrosoRei.pedrosoReiExploitationTerm(BigDecimal("10000000000"),BigDecimal("100000000000"),BigDecimal("10000000000"))
            val maximizeCase = UCT_PedrosoRei.pedrosoReiExploitationTerm(BigDecimal("100000000000"),BigDecimal("10000000000"),BigDecimal("100000000000"))
            minimizeCase should equal (1.0D)
            maximizeCase should equal (minimizeCase)
          }
        }
        "child best is equal to global worst" should {
          "be equal to 0.0" in {
            val minimizeCase = UCT_PedrosoRei.pedrosoReiExploitationTerm(BigDecimal("10000000000"),BigDecimal("100000000000"),BigDecimal("100000000000"))
            val maximizeCase = UCT_PedrosoRei.pedrosoReiExploitationTerm(BigDecimal("100000000000"),BigDecimal("10000000000"),BigDecimal("10000000000"))
            println(s"$minimizeCase $maximizeCase")
            minimizeCase should equal (0.0D)
            maximizeCase should equal (minimizeCase)
          }
        }
      }
      "explorationTerm() ~~not exploitation term~~" when {
        "child visits is zero" should {
          "produce positive infinity" in {
            UCT_PedrosoRei.pedrosoReiExplorationTerm(100,1000,500,math.sqrt(2),100,0) should equal (Double.PositiveInfinity)
          }
        }
        "global best and worst are the same" should {
          "produce positive infinity" in {
            UCT_PedrosoRei.pedrosoReiExplorationTerm(1000,1000,1000,math.sqrt(2),100,50) should equal (Double.PositiveInfinity)
          }
        }
        "child avg is near global best and parent visits are large enough to minimize exploration" should {
          "encourage exploring this promising branch" in {
            val childVisits = 50
            val parentVisits = 10000
            val Cp = math.sqrt(2)
            val minimizeCase = UCT_PedrosoRei.pedrosoReiExplorationTerm(BigDecimal(100),BigDecimal("100000000000"),BigDecimal(200),Cp,parentVisits,childVisits)
            val maximizeCase = UCT_PedrosoRei.pedrosoReiExplorationTerm(BigDecimal("100000000000"),BigDecimal(100),BigDecimal("99999999800"),Cp,parentVisits,childVisits)
            minimizeCase should be > 0.50D
            maximizeCase should be > 0.50D
          }
        }
        "child avg is near global worst and visits are not low" should {
          "be unencouraged to explore this branch" in {
            val childVisits = 50
            val parentVisits = 10000
            val Cp = math.sqrt(2)
            val minimizeCase = UCT_PedrosoRei.pedrosoReiExplorationTerm(BigDecimal(100),BigDecimal("100000000000"),BigDecimal("99999999800"),Cp,parentVisits,childVisits)
            val maximizeCase = UCT_PedrosoRei.pedrosoReiExplorationTerm(BigDecimal("100000000000"),BigDecimal(100),BigDecimal(200),Cp,parentVisits,childVisits)
            println(s"x < 0.5? $minimizeCase $maximizeCase")
            minimizeCase should be < 0.50D
            maximizeCase should be < 0.50D
          }
        }
        "traditional UCT exploration term goes to 1" should {
          "get close to the same value as pedrosoReiExploitationTerm" in {
            val childVisits = 1
            val parentVisits = 1
            val Cp = 1
            val minBest = BigDecimal("100")
            val minWorst = BigDecimal("1000000000")
            val minLocal = (minBest + minWorst) / BigDecimal(2)
            val maxBest = BigDecimal("1000000000")
            val maxWorst = BigDecimal("100")
            val maxLocal = (minBest + minWorst) / BigDecimal(2)
            val minExploit = UCT_PedrosoRei.pedrosoReiExploitationTerm(minBest, minWorst, minLocal)
            val minExplore = UCT_PedrosoRei.pedrosoReiExplorationTerm(minBest, minWorst, minLocal,Cp,parentVisits,childVisits)
            val maxExploit = UCT_PedrosoRei.pedrosoReiExploitationTerm(maxBest, maxWorst, maxLocal)
            val maxExplore = UCT_PedrosoRei.pedrosoReiExplorationTerm(maxBest, maxWorst, maxLocal,Cp,parentVisits,childVisits)
            println(s"$minExploit ~= $minExplore , $maxExploit ~= $maxExplore")
          }
        }
        "parent and child visits are very large" should {
          "get close to the same value as pedrosoReiExploitationTerm" in {
            val childVisits = 50000000L
            val parentVisits = childVisits * 10
            val Cp = math.sqrt(2)
            val minBest = BigDecimal("100")
            val minWorst = BigDecimal("1000000000")
            val minLocal = (minBest + minWorst) / BigDecimal(2)
            val maxBest = BigDecimal("1000000000")
            val maxWorst = BigDecimal("100")
            val maxLocal = (minBest + minWorst) / BigDecimal(2)
            val minExploit = UCT_PedrosoRei.pedrosoReiExploitationTerm(minBest, minWorst, minLocal)
            val minExplore = UCT_PedrosoRei.pedrosoReiExplorationTerm(minBest, minWorst, minLocal,Cp,parentVisits,childVisits)
            val maxExploit = UCT_PedrosoRei.pedrosoReiExploitationTerm(maxBest, maxWorst, maxLocal)
            val maxExplore = UCT_PedrosoRei.pedrosoReiExplorationTerm(maxBest, maxWorst, maxLocal,Cp,parentVisits,childVisits)
            println(s"$minExploit ~= $minExplore , $maxExploit ~= $maxExplore")
          }
        }
      }
    }
    "apply()" when {
      "minimizing" should {
        "monotonically decrease as things get worse" in {
          // "gets worse" defined as
          // "local value moves from being equal to the global best to being equal to the global worst
          val (globalWorst, globalBest) = (BigDecimal.decimal(1000), BigDecimal.decimal(100))
          val Cp = math.sqrt(2)
          val (childVisits, parentVisits) = (100L, 1000L)
          (globalBest to globalWorst by 100).map {
            localBest =>
              val localAvg: BigDecimal = (localBest + globalWorst) / 2
              val reward = UCT_PedrosoRei(globalBest, globalWorst, localBest, localAvg, childVisits, parentVisits, Cp)
              reward
          }.sliding(2).foreach {
            pair =>
              pair(0) should be >= pair(1)
          }
        }
      }
      "maximizing" should {
        "monotonically decrease as things get worse" in {
          // "gets worse" defined as
          // "local value moves from being equal to the global best to being equal to the global worst
          val (globalWorst, globalBest) = (BigDecimal.decimal(100), BigDecimal.decimal(1000))
          val Cp = math.sqrt(2)
          val (childVisits, parentVisits) = (100L, 1000L)
          (globalBest to globalWorst by 100).map {
            localBest =>
              val localAvg: BigDecimal = (localBest + globalWorst) / 2
              val reward = UCT_PedrosoRei(globalBest, globalWorst, localBest, localAvg, childVisits, parentVisits, Cp)
              reward
          }.sliding(2).foreach {
            pair =>
              pair(0) should be >= pair(1)
          }
        }
      }
    }
  }
}

package cse.bdlab.fitzgero.mcts.algorithm.samplingpolicy.banditfunction

import scala.util.Random

import cse.bdlab.fitzgero.mcts.distribution._
import cse.bdlab.fitzgero.mcts.TestTemplate

class SP_UCTTests extends TestTemplate {
  "SP_UCT" when {
    "base cases" when {
      "all zeroes" should {
        "produce a zero-valued Observation" in {
          SP_UCT(RunningDistribution(),0,0,0,0) should equal (Observation(0D))
//          SP_UCT(Observation(0D),0,0,0,0) should equal (Observation(0D))
        }
      }
      "parentVisits" when {
        "is zero when childVisits is nonzero" should {
          "produce a zero-valued Observation" in {
            SP_UCT(RunningDistribution() + Observation(5D),10,0,0,0) should equal (Observation(0D))
          }
        }
      }
      "childVisits" when {
        "is zero when parentVisits is nonzero" should {
          "produce a zero-valued Observation" in {
            SP_UCT(RunningDistribution() + Observation(5D),0,10,0,0) should equal (Observation(0D))
          }
        }
      }
      "Cp" when {
        "when Distribution has a zero-valued variance" when {
          "is zero" should {
            "not influence the observed value in reward evaluation" in {
              println(SP_UCT(RunningDistribution()+ Observation(5D),1,1,0,1).value)
              SP_UCT(RunningDistribution()+Observation(5D),1,1,0,0) should equal (Observation(5D)) // 5
            }
          }
          "is infinite" should {
            "produce an infinite-valued Observation dominated by the same infinity" in {
              SP_UCT(RunningDistribution()+Observation(5D),5,10,Double.PositiveInfinity,1) should equal (Observation(Double.PositiveInfinity))
              SP_UCT(RunningDistribution()+Observation(5D),5,10,Double.NegativeInfinity,1) should equal (Observation(Double.NegativeInfinity))
            }
          }
        }
      }
      "D" when {
        "is zero" should {
          "not influence the observed value in reward evaluation" in {
            SP_UCT(RunningDistribution()+Observation(1D),1,1,1,0) should equal (Observation(1D))
          }
        }
      }
    }
    "impact of Distribution" when {
      "increasing variance" should {
        "encourage exploration" in {
          val random = new Random
          val smallVariance = Iterator.continually(50D).take(50).foldLeft(RunningDistribution()){
            (acc, value) =>
              acc + Observation(value + (0.5 - random.nextDouble))
          }
          val largeVariance = Iterator.continually(50D).take(50).foldLeft(RunningDistribution()){
            (acc, value) =>
              acc + Observation(value + ((0.5 - random.nextDouble) * 50D))
          }
          println(smallVariance)
          println(SP_UCT(smallVariance,100,2000,0.7071,20000))
          println(largeVariance)
          println(SP_UCT(largeVariance,100,2000,0.7071,20000))
        }
      }
    }
  }
}

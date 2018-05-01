package cse.bdlab.fitzgero.mcts.distribution

import cse.bdlab.fitzgero.mcts.TestTemplate

class RunningDistributionTests extends TestTemplate {
  "Distribution" when {
    "constructed without an argument" should {
      "correctly represent an empty distribution" in {
        val test = RunningDistribution()
        test.isEmpty should equal (true)
        test.min.isEmpty should be (true)
        test.max.isEmpty should be (true)
        test.mean.isEmpty should be (true)
        test.variance.isEmpty should be (true)
        test.standardDeviation.isEmpty should be (true)
        test.count should equal (0)
      }
    }
    "constructed with a value" should {
      "correctly represent a singleton Distribution" in {
        val testValue = Observation(4D)
        val test = RunningDistribution() + testValue
        test.min.get should equal (testValue.value)
        test.max.get should equal (testValue.value)
        test.mean.get should equal (testValue.value)
        test.count should equal (1)
        test.variance.get should equal (0D)
        test.standardDeviation.get should equal (0D)
      }
    }
    "+" when {
      "given a distribution and two samples" should {
        "correctly update the Distribution" in {
          val firstValue = Observation(4D)
          val secondValue = Observation(8D)
          val test = RunningDistribution() + firstValue + secondValue
          test.min.get should equal (firstValue.value)
          test.max.get should equal (secondValue.value)
          test.mean.get should equal ((firstValue.value + secondValue.value) / 2)
          test.count should equal (2)
          test.variance.get should equal (4D)
          test.standardDeviation.get should equal (math.sqrt(4D))
        }
      }
      "given a distribution and many new samples" ignore {
        "website numbers to compare with" in {
//          val test = (((((RunningDistribution() + 3) + 21) + 98) + 203 )+ 17) + 9
//          test.min.get should equal(3.0D)
//          test.max.get should equal(203.0D)
//          math.floor(test.mean.get) should equal(58.0D)
//          test.count should equal(6)
//          test.sampleVariance.get should equal (6219.9)
//          math.round(test.standardDeviation.get) should equal (79)
        }
      }
    }
    "++" when {
      "given two small Distributions" ignore {
        "properly combine them" in {
          // ++ not yet implemented
//          val left = Distribution(2) + 4
//          val right = Distribution(6) + 8
//          val result = left ++ right
//          println(result)

        }
      }
    }
  }
}

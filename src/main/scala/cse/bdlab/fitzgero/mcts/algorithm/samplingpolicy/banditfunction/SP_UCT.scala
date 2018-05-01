package cse.bdlab.fitzgero.mcts.algorithm.samplingpolicy.banditfunction

import cse.bdlab.fitzgero.mcts.distribution.{Observation, RunningDistribution}

object SP_UCT {
  def apply(reward: RunningDistribution, childVisits: Long, parentVisits: Long, Cp: Double, D: Double): Observation = {

    if (parentVisits == 0L) {
      Observation(0D)
    } else {

      val exploitation: Double =
        if (childVisits == 0)
          0D
        else
          reward.mean match {
            case None => 0D
            case Some(mean) => mean
          }

      val exploration: Double =
        if (Cp == 0)
          0D
        else if (childVisits == 0)
          Double.MaxValue
        else
          2 * Cp * math.sqrt((2.0D * math.log(parentVisits)) / childVisits)

      val possibleDeviation: Double = {
        val variance = reward.variance.getOrElse(0D)
        if (childVisits == 0)
          math.sqrt(variance)
        else
          math.sqrt(variance + (D / childVisits))
      }

      Observation(exploitation + exploration + possibleDeviation)
    }
  }
}

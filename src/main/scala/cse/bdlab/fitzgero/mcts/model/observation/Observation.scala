package cse.bdlab.fitzgero.mcts.model.observation

sealed trait Observation

object Observation {

  final case object Empty extends Observation

  final case class Single(observation: Double) extends Observation {

  }

  final class AccumulatedMean(
    var observationsSum: Double = 0.0,
    var observedMin: Double = Double.PositiveInfinity,
    var observedMax: Double = Double.NegativeInfinity,
    var count: Int = 0,
  ) extends Observation {

    /**
      * add observations to this accumulator
      * @param that another accumulator to combine
      */
    def add(that: AccumulatedMean): Unit = {
      this.observationsSum +=that.observationsSum
      this.observedMin = math.min(this.observedMin, that.observedMin)
      this.observedMax = math.max(this.observedMax, that.observedMax)
      this.count += that.count
    }

    /**
      * add a single observation to this accumulator
      * @param observation the observation to add
      */
    def add(observation: Double): Unit = {
      this.observationsSum += observation
      if (observation < this.observedMin) this.observedMin = observation
      if (observation > this.observedMax) this.observedMax = observation
      this.count += 1
    }
  }

  object AccumulatedMean {
    /**
      * builds a data structure to observe more than 1 data point
      * as a min, mean, max, and count.
      * @param x a single observation
      * @param y another single observation
      * @return the accumulated mean observation of these observations
      */
    def apply(x: Single, y: Single): AccumulatedMean = {
      val min = math.min(x.observation, y.observation)
      val max = math.max(x.observation, y.observation)
      new AccumulatedMean(x.observation + y.observation, min, max, 2)
    }

    /**
      * combines a Single observation with a new observation to create
      * a AccumulatedMean observation
      * @param x one Single observation
      * @param obs a new observation
      * @return the accumulator representation of observations
      */
    def apply(x: Single, obs: Double): AccumulatedMean = {
      val min = math.min(x.observation, obs)
      val max = math.max(x.observation, obs)
      new AccumulatedMean(x.observation + obs, min, max, 2)
    }
  }

}


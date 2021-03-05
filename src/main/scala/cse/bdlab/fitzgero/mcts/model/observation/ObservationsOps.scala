package cse.bdlab.fitzgero.mcts.model.observation

import cats.Monoid

import cse.bdlab.fitzgero.mcts.model.observation.Observations.ObservationsDoublePrecision

object ObservationsOps {

  implicit val ObsDoubleMonoid: Monoid[ObservationsDoublePrecision] = new Monoid[ObservationsDoublePrecision] {
    def empty: ObservationsDoublePrecision = new ObservationsDoublePrecision()
    def combine(x: ObservationsDoublePrecision, y: ObservationsDoublePrecision): ObservationsDoublePrecision =
      new ObservationsDoublePrecision(
        x.observationsSum + y.observationsSum,
        math.min(x.observedMin, y.observedMin),
        math.max(x.observedMax, y.observedMax),
        x.count + y.count
      )
  }

  implicit class ObsDoubleOpsInstance(observations: Observations) {
    def visits: Int = observations match {
      case mrd: ObservationsDoublePrecision => mrd.count
    }

    def reward: Double = observations match {
      case mrd: ObservationsDoublePrecision => mrd.observationsSum / mrd.count
    }

    def update(observation: Double): Unit = observations match {
      case mrd: ObservationsDoublePrecision =>
        mrd.observationsSum += observation
        if (observation < mrd.observedMin) mrd.observedMin = observation
        if (observation > mrd.observedMax) mrd.observedMax = observation
        mrd.count += 1
    }

    def min: Double = observations match {
      case odp: ObservationsDoublePrecision => odp.observedMin
    }

    def max: Double = observations match {
      case odp: ObservationsDoublePrecision => odp.observedMax
    }

    def mean: Double = observations match {
      case odp: ObservationsDoublePrecision => if (odp.count == 0) 0.0 else odp.observationsSum / odp.count
    }
  }
}

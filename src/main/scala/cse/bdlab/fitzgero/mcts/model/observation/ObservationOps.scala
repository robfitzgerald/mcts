package cse.bdlab.fitzgero.mcts.model.observation

import cats.Monoid

import cse.bdlab.fitzgero.mcts.model.observation.Observation.AccumulatedMean

object ObservationOps {

  implicit val ObservationMonoid: Monoid[Observation] = new Monoid[Observation] {
    def empty: Observation = Observation.Empty
    def combine(x: Observation, y: Observation): Observation =
      x match {
        case Observation.Empty => y
        case xSingle: Observation.Single =>
          y match {
            case Observation.Empty => x
            case ySingle: Observation.Single => AccumulatedMean(xSingle, ySingle)
            case yAcc: AccumulatedMean =>
              yAcc.update(xSingle.observation)
              yAcc
          }
        case xAcc: AccumulatedMean =>
          y match {
            case Observation.Empty => xAcc
            case ySingle: Observation.Single =>
              xAcc.update(ySingle.observation)
              xAcc
            case yAcc: AccumulatedMean =>
              xAcc.add(yAcc)
              xAcc
          }
      }
  }

  implicit class ObservationOpsImpl (self: Observation) {
    def visits: Int = self match {
      case Observation.Empty => 0
      case _: Observation.Single => 1
      case acc: AccumulatedMean => acc.count
    }

    def update(observation: Double): Observation = self match {
      case Observation.Empty =>
        Observation.Single(observation)
      case single: Observation.Single =>
        Observation.AccumulatedMean(single, observation)
      case acc: AccumulatedMean =>
        acc.add(observation)
        acc
    }

    def min: Double = self match {
      case Observation.Empty => Double.NegativeInfinity
      case Observation.Single(observation) => observation
      case mean: AccumulatedMean => mean.observedMin
    }

    def max: Double = self match {
      case Observation.Empty => Double.PositiveInfinity
      case Observation.Single(observation) => observation
      case mean: AccumulatedMean => mean.observedMax
    }

    def mean: Double = self match {
      case Observation.Empty => 0.0
      case Observation.Single(observation) => observation
      case acc: AccumulatedMean => if (acc.count == 0) 0.0 else acc.observationsSum / acc.count
    }
  }
}

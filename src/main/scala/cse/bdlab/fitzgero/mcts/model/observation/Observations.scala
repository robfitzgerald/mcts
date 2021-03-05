package cse.bdlab.fitzgero.mcts.model.observation

sealed trait Observations {
  def count: Int
}

object Observations {
  final class ObservationsDoublePrecision(
    var observationsSum: Double = 0.0,
    var observedMin: Double = Double.PositiveInfinity,
    var observedMax: Double = Double.NegativeInfinity,
    var count: Int = 0,
  ) extends Observations

//  final class ObservationsBigDecimal(
//    var observationsSum: BigDecimal = 0.0,
//    var observedMin: BigDecimal = BigDecimal("9" * 30),
//    var observedMax: BigDecimal = - BigDecimal("9" * 30),
//    var count: Int = 0
//  ) extends Observations
}


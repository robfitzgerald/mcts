//package cse.fitzgero.mcts.math
//
//case class RunningDistributionGeneric[T](
//  min: Option[T] = None,
//  max: Option[T] = None,
//  mean: Option[T] = None,
//  v_k: Option[T] = None,
//  count: Int = 0
//)(implicit f: Fractional[T]) extends Distribution[T] {
//
//  def isEmpty: Boolean = count == 0
//
//  /**
//    * calculate the sample variance of this distribution
//    */
//  lazy val sampleVariance: Option[T] =
//    for {
//      thisV_k <- v_k
//    } yield if (thisV_k == f.zero) f.zero else f.div(thisV_k, f.fromInt(count))
//
//  /**
//    * adds a new observation to this Distribution
//    * @param rhs the next observation
//    * @return a new Distribution
//    */
//  def + (rhs: Observation[T]): RunningDistributionGeneric[T] = {
//    if (this.count == 0) {
//      RunningDistributionGeneric.fromObservation[T](rhs)(f)
//    } else {
//      val nextMin: Option[T] = min map { thisMin => if (f.lt(rhs.value, thisMin)) rhs.value else thisMin }
//      val nextMax: Option[T] = max map { thisMax => if (f.gt(rhs.value, thisMax)) rhs.value else thisMax }
//      val nextCount = count + 1
//      val nextMean: Option[T] = mean map { thisMean => f.plus(thisMean, f.div(f.minus(rhs.value, thisMean), f.fromInt(nextCount))) }
//      val nextV_k: Option[T] = v_k match {
//        case None =>
//          Some(f.zero)
//        case Some(thisV_kValue) =>
//          for {
//            thisMeanValue <- mean
//            nextMeanValue <- nextMean
//          } yield {
//            f.plus(thisV_kValue, f.times(f.minus(rhs.value, thisMeanValue), f.minus(rhs.value, nextMeanValue)))
//          }
//      }
//
//      new RunningDistributionGeneric[T](nextMin, nextMax, nextMean, nextV_k, nextCount)
//    }
//  }
//
//  /**
//    * calculate the standard deviation of this Distribution
//    * with a precision down to 1/epsilon
//    */
//
//  private val epsilon = 1000
//
//  lazy val standardDeviation: Option[T] =
//    for {
//      variance <- sampleVariance
//    } yield f.div(f.fromInt((math.sqrt(f.toDouble(variance)) * epsilon).toInt), f.fromInt(epsilon))
//}
//
//object RunningDistributionGeneric {
//  def fromObservation[T](observation: Observation[T])(fractional: Fractional[T]): RunningDistributionGeneric[T] =
//    RunningDistributionGeneric(Some(observation.value), Some(observation.value), Some(observation.value), Some(fractional.zero), 1)(fractional)
//}
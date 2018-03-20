//package cse.fitzgero.mcts.math
//
//case class Observation[T](value: T)(implicit numeric: Numeric[T]) extends Distribution[T] {
//  def isEmpty = false
//  def min: Option[T] = Some(value)
//  def max: Option[T] = Some(value)
//  def mean: Option[T] = Some(value)
//  def sampleVariance: Option[T] = Some(numeric.zero)
//  def standardDeviation: Option[T] = None
//  def count = 1
//}
//
//object Observation {
//  object DoubleOrderings {
//
//    //  variance scaled by 1000 so the coercion to Int does not strip all meaning from variance values
//    private val epsilon = 1000D
//
//    val distributionByMaxOrdering: Ordering[Observation[Double]] = Ordering.by{ d: Observation[Double] => d.max match { case None => Int.MinValue; case Some(max) => max.toInt } }
//    val distributionByMinOrdering: Ordering[Observation[Double]] = Ordering.by{ d: Observation[Double] => d.min match { case None => Int.MinValue; case Some(min) => min.toInt } }
//    val distributionByMeanOrdering: Ordering[Observation[Double]] = Ordering.by{ d: Observation[Double] => d.mean match { case None => Int.MinValue; case Some(mean) => mean.toInt } }
//
//    /**
//      * use this ordering when higher means and tighter variance is desired
//      * variance scaled by 1000 so the coercion to Int does not strip all meaning from variance values
//      */
//    val distributionByMaximalMeanVarOrdering: Ordering[Observation[Double]] = Ordering.by {
//      d: Observation[Double] =>
//        {
//          for {
//            mean <- d.mean
//            variance <- d.sampleVariance
//          } yield (mean.toInt, (-epsilon * variance).toInt)
//        } match {
//          case None => (Int.MinValue, Int.MinValue)
//          case Some(tuple) => tuple
//        }
//    }
//
//    /**
//      * use this ordering when lower means and tighter variance is desired
//      */
//    val distributionByMinimalMeanVarOrdering: Ordering[Observation[Double]] = Ordering.by {
//      d: Observation[Double] =>
//        {
//          for {
//            mean <- d.mean
//            variance <- d.sampleVariance
//          } yield (-mean.toInt, (-epsilon * variance).toInt)
//        } match {
//          case None => (Int.MinValue, Int.MinValue)
//          case Some(tuple) => tuple
//        }
//    }
//
//
//    //  much easier to use the Ordering.by semantics
//    //  class ObservationMaxValueOrdering extends Ordering[Observation[Double]] {
//    //    override def compare(x: Observation[Double], y: Observation[Double]): Int = {
//    //      val comp = for {
//    //        xMax <- x.max
//    //        yMax <- y.max
//    //      } yield xMax - yMax
//    //      comp match {
//    //        case Some(comparator) =>
//    //          // concerns here if low magnitude negative/positive values might round to zero and change ordering implications. normalized to {-1,0,1}
//    //          if (comparator < 0) -1 else if (comparator > 0) 1 else 0
//    //        case None =>
//    //          // at least one value is empty
//    //          if (x.max.isEmpty && y.max.isEmpty) 0
//    //          else if (x.max.isEmpty) -1
//    //          else 1
//    //      }
//    //    }
//    //  }
//  }
//
//
//}
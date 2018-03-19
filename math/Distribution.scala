package cse.fitzgero.mcts.math



/**
  * computes a running distribution over the inputs provided
  * @see https://math.stackexchange.com/questions/20593/calculate-variance-from-a-stream-of-sample-values
  * @see https://www.johndcook.com/blog/skewness_kurtosis/ (for combining two groups)
  * @param min the lowest value ever observed in this Distribution
  * @param max the highest value ever observed in this Distribution
  * @param mean the current mean for this Distribution
  * @param v_k the accumulator for variance values, where variance = v_k / (k - 1) for the k'th observation
  * @param count the number of values observed in this Distribution
  */
class Distribution (val min: Option[Double] = None, val max: Option[Double] = None, val mean: Option[Double] = None, val v_k: Option[Double] = None, val count: Int = 0) {

  def isEmpty: Boolean = count == 0

  /**
    * calculate the sample variance of this distribution
    */
  lazy val sampleVariance: Option[Double] =
    for {
      thisV_k <- v_k
    } yield if (thisV_k == 0) 0D else thisV_k / count

  /**
    * calculate the standard deviation of this Distribution
    */
  lazy val standardDeviation: Option[Double] =
    for {
      variance <- sampleVariance
    } yield math.sqrt(variance)

  /**
    * adds a new observation to this Distribution
    * @param nextValue the next observation
    * @return a new Distribution
    */
  def + (nextValue: Double): Distribution = {
    val nextMin: Option[Double] = min map { thisMin => if (nextValue < thisMin) nextValue else thisMin }
    val nextMax: Option[Double] = max map { thisMax => if (nextValue > thisMax) nextValue else thisMax }
    val nextCount = count + 1
    val nextMean: Option[Double] = mean map { thisMean => thisMean + ((nextValue - thisMean) / nextCount)}
    val nextV_k: Option[Double] = v_k match {
      case None =>
        Some(0D)
      case Some(thisV_kValue) =>
        for {
          thisMeanValue <- mean
          nextMeanValue <- nextMean
        } yield {
          thisV_kValue + ((nextValue - thisMeanValue) * (nextValue - nextMeanValue))
        }
    }

    new Distribution(nextMin, nextMax, nextMean, nextV_k, nextCount)
  }

  /**
    * adds a new observation to this Distribution
    * @param rhs the next observation
    * @return a new Distribution
    */
  def + (rhs: Distribution): Distribution = {
    if (rhs.count > 1) {
      println("[WARN] attempting to add a 'distribution' with multiple values using the + operator, which is suited only to single observations.")
      this
    } else if (this.count == 0) {
      rhs
    } else {
      rhs.mean match {
        case None => this
        case Some(nextValue) =>
          val nextMin: Option[Double] = min map { thisMin => if (nextValue < thisMin) nextValue else thisMin }
          val nextMax: Option[Double] = max map { thisMax => if (nextValue > thisMax) nextValue else thisMax }
          val nextCount = count + 1
          val nextMean: Option[Double] = mean map { thisMean => thisMean + ((nextValue - thisMean) / nextCount)}
          val nextV_k: Option[Double] = v_k match {
            case None =>
              Some(0D)
            case Some(thisV_kValue) =>
              for {
                thisMeanValue <- mean
                nextMeanValue <- nextMean
              } yield {
                thisV_kValue + ((nextValue - thisMeanValue) * (nextValue - nextMeanValue))
              }
          }

          new Distribution(nextMin, nextMax, nextMean, nextV_k, nextCount)
      }
    }
  }

  // TODO: how to combine v_k? see:
  // https://stats.stackexchange.com/questions/43159/how-to-calculate-pooled-variance-of-two-groups-given-known-group-variances-mean
//  /**
//    * combines two Distributions
//    * @param rhs another distribution
//    * @return the combination of the two Distributions
//    */
//  def ++ (rhs: Distribution): Distribution = {
//    val groupMin = if (this.min < rhs.min) this.min else rhs.min
//    val groupMax = if (this.max > rhs.max) this.max else rhs.max
//    val groupCount = this.count + rhs.count
//    val groupMean = ((this.count * this.mean) + (rhs.count * rhs.mean)) / groupCount
//    val groupV_k = this.v_k * rhs.v_k
//    //    val groupV_k = (this.count * (this.sampleVariance + (this.mean - groupMean))) + (rhs.count * (rhs.sampleVariance + (rhs.mean - groupMean)))
//    new Distribution(groupMin, groupMax, groupMean, groupV_k, groupCount)
//  }

  override def toString: String = {
    {
      for {
        minValue <- min
        maxValue <- max
        meanValue <- mean
        varValue <- sampleVariance
        stdDevValue <- standardDeviation
      } yield f"Distribution: min $minValue%.3f max $maxValue%.3f mean $meanValue%.3f sampleVariance $varValue%.3f standardDeviation $stdDevValue%.3f over $count observations."
    } match {
      case None => s"Empty Distribution"
      case Some(toString) => toString
    }
  }
}

object Distribution {
  def apply(): Distribution = new Distribution()
  def apply(valueOne: Double): Distribution = new Distribution(Some(valueOne),Some(valueOne),Some(valueOne),Some(0),1)

  //  variance scaled by 1000 so the coercion to Int does not strip all meaning from variance values
  private val epsilon = 1000D

  val distributionByMaxOrdering: Ordering[Distribution] = Ordering.by{ d: Distribution => d.max match { case None => Int.MinValue; case Some(max) => max.toInt } }
  val distributionByMinOrdering: Ordering[Distribution] = Ordering.by{ d: Distribution => d.min match { case None => Int.MinValue; case Some(min) => min.toInt } }
  val distributionByMeanOrdering: Ordering[Distribution] = Ordering.by{ d: Distribution => d.mean match { case None => Int.MinValue; case Some(mean) => mean.toInt } }

  /**
    * use this ordering when higher means and tighter variance is desired
    * variance scaled by 1000 so the coercion to Int does not strip all meaning from variance values
    */
  val distributionByMaximalMeanVarOrdering: Ordering[Distribution] = Ordering.by {
    d: Distribution =>
      {
        for {
          mean <- d.mean
          variance <- d.sampleVariance
        } yield (mean.toInt, (-epsilon * variance).toInt)
      } match {
        case None => (Int.MinValue, Int.MinValue)
        case Some(tuple) => tuple
      }
  }

  /**
    * use this ordering when lower means and tighter variance is desired
    */
  val distributionByMinimalMeanVarOrdering: Ordering[Distribution] = Ordering.by {
    d: Distribution =>
      {
        for {
          mean <- d.mean
          variance <- d.sampleVariance
        } yield (-mean.toInt, (-epsilon * variance).toInt)
      } match {
        case None => (Int.MinValue, Int.MinValue)
        case Some(tuple) => tuple
      }
  }


//  much easier to use the Ordering.by semantics
//  class DistributionMaxValueOrdering extends Ordering[Distribution] {
//    override def compare(x: Distribution, y: Distribution): Int = {
//      val comp = for {
//        xMax <- x.max
//        yMax <- y.max
//      } yield xMax - yMax
//      comp match {
//        case Some(comparator) =>
//          // concerns here if low magnitude negative/positive values might round to zero and change ordering implications. normalized to {-1,0,1}
//          if (comparator < 0) -1 else if (comparator > 0) 1 else 0
//        case None =>
//          // at least one value is empty
//          if (x.max.isEmpty && y.max.isEmpty) 0
//          else if (x.max.isEmpty) -1
//          else 1
//      }
//    }
//  }
}
package cse.fitzgero.mcts.math

sealed trait DoublePrecisionDistribution extends Any with Distribution[Double]

/**
  * value class representing a single observation
  * @param value the observation value
  */
class Observation(val value: Double) extends AnyVal with DoublePrecisionDistribution {
  def isEmpty = false
  def min: Option[Double] = Some(value)
  def max: Option[Double] = Some(value)
  def mean: Option[Double] = Some(value)
  def variance: Option[Double] = Some(0D)
  def standardDeviation: Option[Double] = Some(0D)
  def count = 1
}
object Observation {
  def apply(value: Double): Observation = new Observation(value)
}

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
case class RunningDistribution (
  min: Option[Double] = None,
  max: Option[Double] = None,
  mean: Option[Double] = None,
  v_k: Option[Double] = None,
  count: Int = 0) extends DoublePrecisionDistribution {

  def isEmpty: Boolean = count == 0

  /**
    * calculate the sample variance of this distribution
    */
  lazy val variance: Option[Double] =
    for {
      thisV_k <- v_k
    } yield if (thisV_k == 0) 0D else thisV_k / count

  /**
    * calculate the standard deviation of this Distribution
    */
  lazy val standardDeviation: Option[Double] =
    for {
      variance <- variance
    } yield math.sqrt(variance)

  /**
    * adds a new observation to this Distribution
    * @param rhs the next observation
    * @return a new Distribution
    */
  def + (rhs: Observation): RunningDistribution = {
    if (this.count == 0) {
      RunningDistribution(Some(rhs.value),Some(rhs.value),Some(rhs.value),Some(0D),1)
    } else {

      val nextMin: Option[Double] = min map { thisMin => if (rhs.value < thisMin) rhs.value else thisMin }
      val nextMax: Option[Double] = max map { thisMax => if (rhs.value > thisMax) rhs.value else thisMax }
      val nextCount = count + 1
      val nextMean: Option[Double] = mean map { thisMean => thisMean + ((rhs.value - thisMean) / nextCount)}
      val nextV_k: Option[Double] = v_k match {
        case None =>
          Some(0D)
        case Some(thisV_kValue) =>
          for {
            thisMeanValue <- mean
            nextMeanValue <- nextMean
          } yield {
            thisV_kValue + ((rhs.value - thisMeanValue) * (rhs.value - nextMeanValue))
          }
      }

      RunningDistribution(nextMin, nextMax, nextMean, nextV_k, nextCount)

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
        varValue <- variance
        stdDevValue <- standardDeviation
      } yield f"RunningDistribution: min $minValue%.3f max $maxValue%.3f mean $meanValue%.3f sampleVariance $varValue%.3f standardDeviation $stdDevValue%.3f over $count observations."
    } match {
      case None => s"Empty Distribution"
      case Some(toString) => toString
    }
  }
}

object DoublePrecisionDistribution {
  //  variance scaled by 1000 so the coercion to Int does not strip all meaning from variance values
  // thus, our epsilon is 1/epsilonDenominator
  private val epsilonDenominator = 1000D

  val distributionByMaxOrdering: Ordering[DoublePrecisionDistribution] = Ordering.by{ d: DoublePrecisionDistribution => d.max match { case None => Int.MinValue; case Some(max) => (epsilonDenominator * max).toInt } }
  val distributionByMinOrdering: Ordering[DoublePrecisionDistribution] = Ordering.by{ d: DoublePrecisionDistribution => d.min match { case None => Int.MinValue; case Some(min) => (epsilonDenominator * min).toInt } }
  val distributionByMeanOrdering: Ordering[DoublePrecisionDistribution] = Ordering.by{ d: DoublePrecisionDistribution => d.mean match { case None => Int.MinValue; case Some(mean) => (epsilonDenominator * mean).toInt } }

  /**
    * use this ordering when higher means and tighter variance is desired
    * variance scaled by 1000 so the coercion to Int does not strip all meaning from variance values
    */
  val distributionByMaximalMeanVarOrdering: Ordering[DoublePrecisionDistribution] = Ordering.by {
    d: DoublePrecisionDistribution =>
      {
        for {
          mean <- d.mean
          variance <- d.variance
        } yield (mean.toInt, (-epsilonDenominator * variance).toInt)
      } match {
        case None => (Int.MinValue, Int.MinValue)
        case Some(tuple) => tuple
      }
  }

  /**
    * use this ordering when lower means and tighter variance is desired
    */
  val distributionByMinimalMeanVarOrdering: Ordering[DoublePrecisionDistribution] = Ordering.by {
    d: DoublePrecisionDistribution =>
      {
        for {
          mean <- d.mean
          variance <- d.variance
        } yield (-mean.toInt, (-epsilonDenominator * variance).toInt)
      } match {
        case None => (Int.MinValue, Int.MinValue)
        case Some(tuple) => tuple
      }
  }
}

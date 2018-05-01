package cse.bdlab.fitzgero.mcts.example.mckdv.pedrosorei

import cse.bdlab.fitzgero.mcts.algorithm.samplingpolicy.banditfunction.UCT_PedrosoRei.{Maximize, Minimize}
import org.rogach.scallop._

object MCKDVPedrosoReiApp extends App {

  class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
    val timeRange = opt[String](default = Some("1000,5000"))
    val nRange = opt[String](default = Some("10,10"))
    val kRange = opt[String](default = Some("4,4"))
    val trials = opt[Int](default = Some(5))
    val costBound = opt[Int](default = Some(50))
    val objective = opt[String](default = Some("maximize"))
    verify()
  }

  def parseRange(s: String): Seq[Int] = {
    val tokens = s.split(",").map{_.toInt}
    if (tokens.length == 1) tokens
    else tokens.sliding(2).flatMap{tup => tup(0) to tup(1)}.toSeq
  }

  val conf = new Conf(args)

  val timeRange = conf.timeRange().split(",").map{_.toInt}.toSeq
  val nRange = parseRange(conf.nRange())
  val kRange = parseRange(conf.kRange())
  val trials = conf.trials()
  val costBound = conf.costBound()

  def orderOfMagnitudeToOptimal(searchCost: BigDecimal, optimalCost: BigDecimal): Double = {
    (searchCost/optimalCost).toDouble
  }

  val random = scala.util.Random
  val experimentsDurMinutes: Double =
  (
  timeRange.map {
    _ * nRange.size * kRange.size
  }.sum * trials
  ) / (1000D * 60D)

  println(s"\ntimes: $timeRange n: $nRange k: $kRange trials: $trials costBound: $costBound objective: ${conf.objective()}")
  println(f"expected duration of experiments: $experimentsDurMinutes%.2f minutes.\n")
  println("n,k,timeBudget,avgOptimalCost,avgSearchCost,orderFromOptimal,completeSolutions,foundTrueOptimal,avgIterations")

  for {
    timeBudget <- timeRange
    n <- nRange
    k <- kRange
  } {
    val lowerBounds = BigDecimal(0)
    val upperBounds = BigDecimal(n * costBound * 2)
    val objective =
      if (conf.objective() == "maximize") Maximize(lowerBounds, upperBounds)
      else if (conf.objective() == "minimize") Minimize(lowerBounds, upperBounds)
      else throw new IllegalArgumentException("invalid objective name")
    val experimentRunner = MCKDVPedrosoReiExperiment(random, costBound, objective)
    val result = experimentRunner.run(n,k,trials,timeBudget)
    println(f"$n,$k,$timeBudget,${result.avgOptimalCost},${result.avgSearchCost},${orderOfMagnitudeToOptimal(result.sumSearchCost, result.sumOptimalCostBaseline)}%.2f,${result.completeSolutions},${result.optimalSolutions},${result.avgIterations}")
  }
}

package cse.bdlab.fitzgero.mcts.example.mckdv.standardmcts

import cse.bdlab.fitzgero.mcts.TestTemplate

class MCKDVExperimentDriver extends TestTemplate {
  "MCKDVExperiment" when {
    "called with a multiset with 2 sets, each with 2 items" should {
      "find the optimal" in {

        ////////////////////////
        // EXPERIMENT PARAMETERS
        val costBound = 50 // the highest cost we get from a dependency, except the optimal, which will be 2 * costBound
        // range for duration of tree building and search
        val lowerT = 1000
        val upperT = 1000
        val tStep = 1000
        // range for # of multisets
        val lowerN = 10
        val upperN = 10
        // range for # of items per set
        val lowerK = 4
        val upperK = 4
        // number of times to run experiment per parameter configuration
        val trials = 5
        ////////////////////////

        val random = scala.util.Random
        val experimentsDurMinutes: Double =
          (
            (lowerT to upperT by tStep)
            .map {
              _ * (lowerN to upperN).size * (lowerK to upperK).size
            }.sum * trials
          ) / (1000D * 60D)

        println(f"expected duration of experiments: $experimentsDurMinutes%.1f minutes.")
        //println(s"running $numExperiments experiments with n={1 to $upperN}, k={1 to $upperK}, $trials trials per experiment, time budget of ${timeBudget/1000} seconds, costs in range [0,$costBound) with optimal costs set to ${2 * costBound}")

        println("n,k,timeBudget,avgOptimalCost,avgSearchCost,percentOfOptimal")
        for {
          timeBudget <- lowerT to upperT by tStep
          n <- lowerN to upperN
          k <- lowerK to upperK
        } {
          val experimentRunner = MCKDVStandardMCTSExperiment(random, costBound)
          val result = experimentRunner.run(n,k,trials,timeBudget)
          println(f"$n,$k,$timeBudget,${result.avgOptimalCost},${result.avgSearchCost},${(result.sumSearchCost/result.sumOptimalCost) * 100D}%.2f%%")
        }

      }
    }
  }
}

package cse.bdlab.fitzgero.mcts.example.tictactoe

import cse.bdlab.fitzgero.mcts.algorithm.samplingpolicy.distribution.UCTDistributionSPMCTSReward
import cse.bdlab.fitzgero.mcts.TestTemplate

class TicTacToeDistributionSolverTests extends TestTemplate {
  "TicTacToeDistributionSP_UCTSolver" when {
    "run with exploitation coefficients" should {
      "tend to exploit knowledge more" in {
        val solver = TicTacToeDistributionSP_UCTSolver(
          duration = 30000L,
          seed = 1L
//          coefficients = UCTDistributionSPMCTSReward.Exploitation
        )
        val tree = solver.run()
        //        println(solver.bestGame(tree))
        //        println(tree.printBestTree(printDepth = 9, solver.evaluate))
        println(tree.printTree(printDepth = 2))
      }
    }
    "run with balanced coefficients" should {
      "balance exploitation and exploration" in {
        val solver = TicTacToeDistributionSP_UCTSolver(
          duration = 30000L,
          seed = 1L
//          coefficients = UCTDistributionSPMCTSReward.Balanced
        )
        val tree = solver.run()
        //        println(solver.bestGame(tree))
        //        println(tree.printBestTree(printDepth = 9, solver.evaluate))
        println(tree.printTree(printDepth = 2))
      }
    }
    "run with exploration coefficients" should {
      "tend to explore more" in {
        val solver = TicTacToeDistributionSP_UCTSolver(
          duration = 30000L,
          seed = 1L
//          coefficients = UCTDistributionSPMCTSReward.Exploration
        )
        val tree = solver.run()
        //        println(solver.bestGame(tree))
        //        println(tree.printBestTree(printDepth = 9, solver.evaluate))
        println(tree.printTree(printDepth = 2))
      }
    }
  }
}

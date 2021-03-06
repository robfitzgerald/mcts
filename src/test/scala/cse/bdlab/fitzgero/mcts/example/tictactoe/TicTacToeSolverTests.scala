package cse.bdlab.fitzgero.mcts.example.tictactoe

import cse.bdlab.fitzgero.mcts.TestTemplate

class TicTacToeSolverTests extends TestTemplate {
  "TicTacToeSolver" when {
    "run with defaults" should {
      "find the full tree and optimal moves for both players" in {
        val solver = TicTacToeSolver(
          duration = 1000L,
          seed = 1L,
          Cp = 0.717D
        )
        val tree = solver.run()
//        println(solver.bestGame(tree))
//        println(tree.printBestTree(printDepth = 9, solver.evaluate))
        println(tree.printTree(printDepth = 2))
      }
    }
  }
}

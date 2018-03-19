package cse.fitzgero.mcts.example

import java.time.Instant

import cse.fitzgero.mcts.core._
import cse.fitzgero.mcts.example.TicTacToe.Board._
import cse.fitzgero.mcts.example.TicTacToe._
import cse.fitzgero.mcts.math.Distribution
import cse.fitzgero.mcts.variant._


class TicTacToeDistributionSolver(
                      seed: Long = 0L,
                      duration: Long = 5000L) extends RewardDistributionStandardMCTS[Board, Move] {

  override def applyAction(state: Board, action: Move): Board = state.applyMove(action)

  // we are creating the best move set for X
  override def evaluateTerminal(state: Board): Distribution =
    Board.gameState(state) match {
      case Stalemate => Distribution(0D)
      case XWins => Distribution(1D) // if (state.currentPlayer == X) 1D else 0D
      case OWins => Distribution(0D) // if (state.currentPlayer == O) 1D else 0D
      case _ => throw new IllegalStateException("evaluating a non-terminal board state")
    }

  def getDecisionCoefficients(tree: Tree): Coefficients = Coefficients()
  def getSearchCoefficients(tree: Tree): Coefficients = Coefficients(0D)

  override def generatePossibleActions(state: Board): Seq[Move] = Board.possibleMoves(state)

  override def selectAction(actions: Seq[Move]): Option[Move] = actionSelection.selectAction(actions)

  override def stateIsNonTerminal(state: Board): Boolean =
    Board.gameState(state) match {
      case Turn(_) => true
      case _ => false
    }

  override def startState: Board = Board(X)
  override def random: RandomGenerator = new BuiltInRandomGenerator(Some(seed))
//  override val samplingMethod =  UCTScalarStandardReward()
  override val terminationCriterion = TimeTermination(Instant.now, duration)
  override val actionSelection = RandomSelection(random, generatePossibleActions)

}

object TicTacToeDistributionSolver {
  def apply(): TicTacToeDistributionSolver = new TicTacToeDistributionSolver()
  def apply(seed: Long, duration: Long, Cp: Double): TicTacToeDistributionSolver =
    new TicTacToeDistributionSolver(seed, duration)
}

package cse.fitzgero.mcts.example.tictactoe

import java.time.Instant

import cse.fitzgero.mcts.algorithm.samplingpolicy.scalar.UCTScalarStandardReward
import cse.fitzgero.mcts.core._
import cse.fitzgero.mcts.core.terminationcriterion.TimeTermination
import cse.fitzgero.mcts.example.tictactoe.TicTacToe.Board._
import cse.fitzgero.mcts.example.tictactoe.TicTacToe._
import cse.fitzgero.mcts.variant._


class TicTacToeSolver(val seed      : Long = 0L,
                      val timeBudget: Long = 5000L) extends StandardMCTS[Board, Move] {

  override def applyAction(state: Board, action: Move): Board = state.applyMove(action)

  // we are creating the best move set for X
  override def evaluateTerminal(state: Board): Double =
    Board.gameState(state) match {
      case Stalemate => 0D
      case XWins => 1D // if (state.currentPlayer == X) 1D else 0D
      case OWins => 0D // if (state.currentPlayer == O) 1D else 0D
      case _ => throw new IllegalStateException("evaluating a non-terminal board state")
    }

  override def getDecisionCoefficients(tree: Tree): Coefficients = UCTScalarStandardReward.DecisionCoefficient
  override def getSearchCoefficients(tree: Tree): Coefficients = UCTScalarStandardReward.ExplorationCoefficient

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
  override val terminationCriterion: TimeTermination[Board,Move,Tree] = TimeTermination[Board,Move,Tree](timeBudget)
  override val actionSelection = RandomSelection(random, generatePossibleActions)

}

object TicTacToeSolver {
  def apply(): TicTacToeSolver = new TicTacToeSolver()
  def apply(seed: Long, duration: Long, Cp: Double): TicTacToeSolver =
    new TicTacToeSolver(seed, duration)
}
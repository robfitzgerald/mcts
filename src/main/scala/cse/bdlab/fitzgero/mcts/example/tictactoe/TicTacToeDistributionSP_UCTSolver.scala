package cse.bdlab.fitzgero.mcts.example.tictactoe

import java.time.Instant

import cse.bdlab.fitzgero.mcts.algorithm.samplingpolicy.distribution.UCTDistributionSPMCTSReward
import cse.bdlab.fitzgero.mcts.algorithm.samplingpolicy.distribution.UCTDistributionSPMCTSReward._
import cse.bdlab.fitzgero.mcts.core._
import cse.bdlab.fitzgero.mcts.core.terminationcriterion.TimeTermination
import cse.bdlab.fitzgero.mcts.distribution.{DoublePrecisionDistribution, Observation, RunningDistribution}
import cse.bdlab.fitzgero.mcts.example.tictactoe.TicTacToe.Board._
import cse.bdlab.fitzgero.mcts.example.tictactoe.TicTacToe._
import cse.bdlab.fitzgero.mcts.variant._

class TicTacToeDistributionSP_UCTSolver(
                      val seed      : Long = 0L,
                      val timeBudget: Long = 5000L) extends RewardDistributionSPMCTS[Board, Move] {

  override def applyAction(state: Board, action: Move): Board = state.applyMove(action)

  // we are creating the best move set for X
  override def evaluateTerminal(state: Board): Update =
    Board.gameState(state) match {
      case Stalemate => Observation(0D)
      case XWins => Observation(1D) // if (state.currentPlayer == X) 1D else 0D
      case OWins => Observation(0D) // if (state.currentPlayer == O) 1D else 0D
      case _ => throw new IllegalStateException("evaluating a non-terminal board state")
    }

  def getSearchCoefficients(tree: Tree): Coefficients = Balanced
  def getDecisionCoefficients(tree: Tree): Coefficients = Coefficients(0D,0D)

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

object TicTacToeDistributionSP_UCTSolver {
  def apply(): TicTacToeDistributionSP_UCTSolver = new TicTacToeDistributionSP_UCTSolver()
  def apply(seed: Long, duration: Long): TicTacToeDistributionSP_UCTSolver =
    new TicTacToeDistributionSP_UCTSolver(seed, duration)
}

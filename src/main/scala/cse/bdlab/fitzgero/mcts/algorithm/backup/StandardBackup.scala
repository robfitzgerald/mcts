package cse.bdlab.fitzgero.mcts.algorithm.backup

import scala.annotation.tailrec

import cse.bdlab.fitzgero.mcts.MonteCarloTreeSearch

trait StandardBackup[S,A] extends MonteCarloTreeSearch[S,A] {
  @tailrec
  override protected final def backup(node: Tree, coefficients: Coefficients, delta: Update): Tree = {
    node.parent() match {
      case None =>
        node.update(delta, coefficients)
        node
      case Some(parent) =>
        // v has a parent, so we want to update v and recurse on parent
        node.update(delta, coefficients)
        backup(parent, coefficients, delta)
    }
  }
}

package cse.fitzgero.mcts.algorithm.backup

import scala.annotation.tailrec

import cse.fitzgero.mcts.MonteCarloTreeSearchPedrosoRei

trait BackupPedrosoRei[S,A] extends MonteCarloTreeSearchPedrosoRei[S,A] {
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

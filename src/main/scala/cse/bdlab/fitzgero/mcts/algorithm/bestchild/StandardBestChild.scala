package cse.bdlab.fitzgero.mcts.algorithm.bestchild

import cse.bdlab.fitzgero.mcts.MonteCarloTreeSearch

trait StandardBestChild[S,A] extends MonteCarloTreeSearch[S,A] {
  override protected final def bestChild(node: Tree, coefficients: Coefficients)(implicit ordering: Ordering[Reward]): Option[Tree] = {
    if (node.hasNoChildren) { None }
    else {
      val children = node.childrenNodes.values map {
        tree: Tree => (tree.reward(coefficients), tree)
      }
      val bestChild = children.maxBy{_._1}._2
      Some(bestChild)
    }
  }
}

package cse.bdlab.fitzgero.mcts.tree

object TestAssets {
  trait SimpleTree {
    type State = String
    type Action = String
    type TestReward = Double

    class Tree(override val state: State, override val action: Option[Action], var rewardStore: TestReward) extends MonteCarloTree[State,Action,TestReward,TestReward,Any,Tree] {
      override def reward(coefficients: Any): TestReward = {
        rewardStore
      }
      override def update(simulationResult: TestReward, coefficients: Any): Unit = {
        visits += 1
        rewardStore += simulationResult
      }
      override def treeSpecificPrintData: String = f"$rewardStore%.2f reward from $visits visits"
    }

    val tree = new Tree("born", None, 10D)
    val newChild = new Tree("a skier", Some("goes skiing"), 2000D)
  }
}

package cse.bdlab.fitzgero.mcts.core.terminationcriterion

import cse.bdlab.fitzgero.mcts.tree.MCTreeStandardReward

object TestAssets {
  type S = Seq[Int]
  type A = Int
  type N = MCTreeStandardReward[S,A]
  trait ComboIterAND {
    val tree: MCTreeStandardReward[S,A] = MCTreeStandardReward[S,A](Seq(), None)
    val test: CombinedTermination[S,A,N] = CombinedTermination[S,A,N](
      IterationTermination(500), IterationTermination(600), CombinedTermination.AND
    )
  }
  trait ComboIterOR {
    val tree: MCTreeStandardReward[S,A] = MCTreeStandardReward[S,A](Seq(), None)
    val test: CombinedTermination[S,A,N] = CombinedTermination[S,A,N](
      IterationTermination(500), IterationTermination(600), CombinedTermination.OR
    )
  }
}

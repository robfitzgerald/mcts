package cse.bdlab.fitzgero.mcts.core.terminationcriterion

import cse.bdlab.fitzgero.mcts.TestTemplate

class CombinedTerminationTests extends TestTemplate {
  "CombinedTermination" when {
    "combining via AND" should {
      "succeed when visits < the lower numbered combined op" in new TestAssets.ComboIterAND {
        tree.visits = 400
        test.withinComputationalBudget(tree) should equal (true)
      }
      "fail when higher than the lower numbered combined conditions" in new TestAssets.ComboIterAND {
        tree.visits = 501
        test.withinComputationalBudget(tree) should equal (false)
      }
      "fail when higher than the both conditions" in new TestAssets.ComboIterAND {
        tree.visits = 601
        test.withinComputationalBudget(tree) should equal (false)
      }
    }
    "combining via OR" should {
      "succeed when visits < the lower numbered combined op" in new TestAssets.ComboIterOR {
        tree.visits = 400
        test.withinComputationalBudget(tree) should equal (true)
      }
      "succeed when higher than the lower numbered combined conditions" in new TestAssets.ComboIterOR {
        tree.visits = 501
        test.withinComputationalBudget(tree) should equal (true)
      }
      "fail when higher than the both conditions" in new TestAssets.ComboIterOR {
        tree.visits = 601
        test.withinComputationalBudget(tree) should equal (false)
      }
    }
  }
}

package cse.bdlab.fitzgero.mcts.core.terminationcriterion

import cse.bdlab.fitzgero.mcts.tree.MonteCarloTree

class CombinedTermination [S,A,N <: MonteCarloTree[S,A,_,_,_,_]] (
  a: TerminationCriterion[S,A,N],
  b: TerminationCriterion[S,A,N],
  op: CombinedTermination.TerminationCombineOp) extends TerminationCriterion[S,A,N] {

  /**
    * initializes each criteria
    */
  override def init(): Unit = {
    a.init()
    b.init()
  }

  /**
    * tests both criteria for being within budget, based on the provided binary operation
    * @param monteCarloTree tree state
    * @return true if we can continue to compute
    */
  override def withinComputationalBudget(monteCarloTree: N): Boolean = {
    op.combine(a.withinComputationalBudget(monteCarloTree), b.withinComputationalBudget(monteCarloTree))
  }

  override def toString: String = s"CombinedTermination($a, $b)"
}

object CombinedTermination {

  /**
    * a small set of binary boolean operators to use with the CombinedTermination class
    */
  sealed trait TerminationCombineOp {
    def combine(a: Boolean, b: Boolean): Boolean
  }
  case object AND extends TerminationCombineOp {
    override def combine(a: Boolean, b: Boolean): Boolean = a && b
  }
  case object OR extends TerminationCombineOp {
    override def combine(a: Boolean, b: Boolean): Boolean = a || b
  }

  /**
    * given two termination criteria, creates a CombinedTermination object, with a default "AND" binary combine operation
    * @param a a termination criteria
    * @param b another termination criteria
    * @return a combined termination criteria
    */
  def apply[S,A,N <: MonteCarloTree[S,A,_,_,_,_]](
    a: TerminationCriterion[S,A,N],
    b: TerminationCriterion[S,A,N],
    op: TerminationCombineOp = CombinedTermination.AND): CombinedTermination[S,A,N] = new CombinedTermination[S,A,N](a,b,op)

  /**
    * provides predefined instances of CombinationIteration constructors
    */
  object Predef {
    def TimeAndIteration[S,A,N](endTime: Long, iterationThreshold: Long): CombinedTermination[S,A,N] =
      new CombinedTermination[S,A,N](new TimeTermination[S,A,N](endTime), new IterationTermination[S,A,N](iterationThreshold), CombinedTermination.AND)

    def TimeOrIteration[S,A,N](endTime: Long, iterationThreshold: Long): CombinedTermination[S,A,N] =
      new CombinedTermination[S,A,N](new TimeTermination[S,A,N](endTime), new IterationTermination[S,A,N](iterationThreshold), CombinedTermination.OR)
  }
}
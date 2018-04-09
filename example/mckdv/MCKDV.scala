package cse.fitzgero.mcts.example.mckdv

object MCKDV {

  trait HasCost { def cost: Int }
  def costsOf(xs: Seq[HasCost]): BigDecimal = xs.map{n => BigDecimal.decimal(n.cost)}.sum

  case class Dependency(cost: Int, dst: Choice) extends HasCost {
    override def toString: String = s"Dependency(cost=$cost, dst=$dst)"
  }
  case class Choice(label: Int, cost: Int) extends HasCost {
    override def toString: String = s"Choice(label=$label, cost=$cost)"
  }

  type Dependencies = Map[Choice, List[Dependency]]
  type Multiset = Set[Set[Choice]]
  type Problem = (Multiset, Dependencies)
  type Selection = Set[Choice]

  implicit class ProblemOps(p: Problem) {
    def multiset: Multiset = p._1
    def dependencies: Dependencies = p._2
  }

  def costOfSelection(selection: Selection, dependencies: Dependencies): BigDecimal = {
    val baseChoiceCosts: BigDecimal = costsOf(selection.toList)
    val depsCosts: BigDecimal = selection.toList.map {
      choice =>
        val deps = dependencies(choice).filter{dep => selection(dep.dst)}
        costsOf(deps)
    }.sum
    baseChoiceCosts + depsCosts
  }
}

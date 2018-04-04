package cse.fitzgero.mcts.example.mckdv

object MCKDV {

  trait HasCost { def cost: Int }
  def costsOf(xs: Seq[HasCost]): Int = xs.map{_.cost}.sum

  case class Dependency(cost: Int, dst: Choice) extends HasCost
  case class Choice(cost: Int) extends HasCost
  type Selection = Map[Choice, List[Dependency]]

  def costOfSelection(selection: Selection): Int = {
    val allDependencies: Int = (
      for {
        sel <- selection
        dep <- sel._2
        if selection.isDefinedAt(dep.dst)
      } yield dep.cost
    ).sum
    val baseCost: Int = selection.foldLeft(0){
      (acc, sel) =>
        acc + sel._1.cost
    }
    baseCost + allDependencies
  }
}

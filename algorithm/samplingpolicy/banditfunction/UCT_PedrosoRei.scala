package cse.fitzgero.mcts.algorithm.samplingpolicy.banditfunction

/**
  * UCT redesigned to solve combinatorial problems
  * from J.P. Pedroso and R. Rei, "Tree Search and Simulation", Applied Simulation and Optimization, Springer International Publishing, pp 119-131, 2015.
  */
object UCT_PedrosoRei {
  /**
    * evaluates UCT with modifications for combinatorial search, shown as equation 1 on page 113
    * @param globalBestSimulation the real objective function evaluation of the best simulation in the whole tree
    * @param globalWorstSimulation the real objective function evaluation of the worst simulation in the whole tree
    * @param childBestSimulation the real objective function evaluation of the best simulation from this child and below
    * @param childAverageSimulation the real objective function evaluation of the average simulation from this child and below
    * @param childVisits the number of simulations run on this child and any of its children
    * @param parentVisits the number of simulations run on the parent node
    * @param Cp the exploration coefficient, typically math.sqrt(2)
    * @return
    */
  def apply(globalBestSimulation: BigDecimal,
            globalWorstSimulation: BigDecimal,
            childBestSimulation: BigDecimal,
            childAverageSimulation: BigDecimal,
            childVisits: Long,
            parentVisits: Long,
            Cp: Double): Double = {

    val X = pedrosoReiExploitationTerm(globalBestSimulation, globalWorstSimulation, childBestSimulation)
    val E = pedrosoReiExplorationTerm(globalBestSimulation, globalWorstSimulation, childAverageSimulation, Cp, parentVisits, childVisits)

    X + E
  }


  /**
    * exploitation for combinatorial search, shown as equation 3 on pg 115
    * @param globalBestSimulation
    * @param globalWorstSimulation
    * @param childBestSimulation
    * @return
    */
  def pedrosoReiExploitationTerm(globalBestSimulation: BigDecimal, globalWorstSimulation: BigDecimal, childBestSimulation: BigDecimal): Double = {
    val a: Double = ((globalWorstSimulation - childBestSimulation) / (globalWorstSimulation - globalBestSimulation)).toDouble
    val numer: Double = math.pow(math.E, a) - 1D
    val denom: Double = math.E - 1D
    if (denom != 0) numer / denom else 0D
  }

  /**
    * exploration for combinatorial search, shown as equations 4 and 5 on pg 116
    * @param globalBestSimulation
    * @param globalWorstSimulation
    * @param childAverageSimulation
    * @return
    */
  def pedrosoReiExplorationTerm(globalBestSimulation: BigDecimal, globalWorstSimulation: BigDecimal, childAverageSimulation: BigDecimal, Cp: Double, parentVisits: Long, childVisits: Long): Double = {
    val a: Double = ((globalWorstSimulation - childAverageSimulation) / (globalWorstSimulation - globalBestSimulation)).toDouble
    val numer: Double = math.pow(math.E, a) - 1D
    val denom: Double = math.E - 1D
    val XBar = if (denom != 0) numer / denom else 0D
    val E = uctExploration(Cp, parentVisits, childVisits)
    XBar * E
  }

  def uctExploration(Cp: Double, parentVisits: Long, childVisits: Long): Double = {
    if (Cp == 0)
      0D
    else if (childVisits == 0)
      Double.MaxValue
    else
      2 * Cp * math.sqrt((2.0D * math.log(parentVisits)) / childVisits)
  }

}

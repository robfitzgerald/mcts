package cse.bdlab.fitzgero.mcts.model.value.costfunction

/**
  * UCT redesigned to solve combinatorial problems
  * from J.P. Pedroso and R. Rei, "Tree Search and Simulation", Applied Simulation and Optimization, Springer International Publishing, pp 119-131, 2015.
  */
object UCTPedrosoRei {

  /**
    * evaluates UCT with modifications for combinatorial search, shown as equation 1 on page 113
    * @param globalBest the real objective function evaluation of the best simulation in the whole tree
    * @param globalWorst the real objective function evaluation of the worst simulation in the whole tree
    * @param childBest the real objective function evaluation of the best simulation from this child and below
    * @param childMean the real objective function evaluation of the average simulation from this child and below
    * @param childVisits the number of simulations run on this child and any of its children
    * @param parentVisits the number of simulations run on the parent node
    * @param Cp the exploration coefficient, typically math.sqrt(2)
    * @return
    */
  def apply(globalBest: Double,
    globalWorst       : Double,
    childBest         : Double,
    childMean         : Double,
    childVisits       : Long,
    parentVisits      : Long,
    Cp                : Double): Double = {

    val X = pedrosoReiExploitationTerm(globalBest, globalWorst, childBest)
    val E = pedrosoReiExplorationTerm(globalBest, globalWorst, childMean, Cp, parentVisits, childVisits)

    val result = X + E
    result
  }


  /**
    * exploitation for combinatorial search, shown as equation 3 on pg 115. desires a finite reward value in the range [0,1]
    * @param globalBestSimulation the best simulation result seen
    * @param globalWorstSimulation the worst simulation result seen
    * @param childBestSimulation the best simulation result the child node has seen
    * @return
    */
  def pedrosoReiExploitationTerm(globalBestSimulation: Double, globalWorstSimulation: Double, childBestSimulation: Double): Double = {
    if (globalBestSimulation == globalWorstSimulation) 0D
    else {
      val result: Double = pedrosoReiXTerm(globalBestSimulation,globalWorstSimulation,childBestSimulation)
      result
    }
  }

  /**
    * exploration for combinatorial search, shown as equations 4 and 5 on pg 116. we desire a reward value in the range [0,inf)
    * @param globalBestSimulation the best simulation result seen
    * @param globalWorstSimulation the worst simulation result seen
    * @param childAverageSimulation the average of simulation result values that the child node has seen
    * @return
    */
  def pedrosoReiExplorationTerm(globalBestSimulation: Double, globalWorstSimulation: Double, childAverageSimulation: Double, Cp: Double, parentVisits: Long, childVisits: Long): Double = {
    if (Cp == 0) {
      0D
    } else if (globalBestSimulation == globalWorstSimulation) {
      Double.PositiveInfinity
    } else {
      val XBar: Double = pedrosoReiXTerm(globalBestSimulation, globalWorstSimulation, childAverageSimulation)
      val E: Double = uctExploration(Cp, parentVisits, childVisits)
      val result = XBar * E
      result
    }
  }

  /**
    * used in both exploration and exploitation terms but differing by max + mean child values
    * @param globalBest the globally-observed best search value
    * @param globalWorst the globally-observed worst search value
    * @param localValue either the max or mean locally-observed value
    * @return "X" term
    */
  def pedrosoReiXTerm(globalBest: Double, globalWorst: Double, localValue: Double): Double = {
    val a: Double = (globalWorst - localValue) / (globalWorst - globalBest)
    val numer: Double = math.pow(math.E, a) - 1D
    val denom: Double = math.E - 1D
    val result: Double = if (denom != 0) numer / denom else 0D
    result
  }

  /**
    * traditional UCT exploration term
    * @param Cp exploration coefficient
    * @param parentVisits number of times parent node has been visited
    * @param childVisits number of times child node has been visited
    * @return UCT exploration value
    */
  def uctExploration(Cp: Double, parentVisits: Long, childVisits: Long): Double = {
    if (parentVisits == 0L) { // math.log(0) == -inf
      0D
    } else if (childVisits == 0L) { // div/0!
      Double.PositiveInfinity
    } else {
      val result: Double = Cp * math.sqrt(math.log(parentVisits) / childVisits)
      result
    }
  }
}

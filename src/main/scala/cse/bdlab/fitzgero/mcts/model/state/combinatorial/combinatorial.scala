package cse.bdlab.fitzgero.mcts.model.state

import scala.collection.BitSet

package object combinatorial {

  /**
    * a state is internally represented by a BitSet.
    *
    * in the case of standard optimization, the set of select-able items
    * is replicated across subsets of the BitSet, where each subset has
    * exactly one item selected.
    *
    * in the case of multiset optimization, the BitSet is sliced into
    * the parts where each part may be differently-sized.
    */
  type ChoiceState = BitSet
  type MultiChoiceState = BitSet

  /**
    * each state is partitioned into subsets of the state, to model
    * each choice in a standard knapsack problem, or each subset in a multi-choice problem
    */
  type StatePartitionSizes = Array[Int]

  /**
    * it is useful to maintain a copy of the partition sizes in an accumulative
    * form for some algorithms.
    */
  type StatePartitionOffsets = Array[Int]

  /**
    * we apply the intersection of a StatePartition mask and a StateOffset
    * in order to fetch the values that have been selected in a state
    */
  type StatePartitionMasks = Array[BitSet]
}

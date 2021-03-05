package cse.bdlab.fitzgero.mcts.model.state.combinatorial

import scala.collection.BitSet

case class MultiChoice(
  slots: Int,
  actions: Array[Int],
  statePartitionMasks: StatePartitionMasks,
  statePartitionOffsets: StatePartitionOffsets
)

object MultiChoice {

  /**
    * creates the assets used when working with a multi choice problem representation
    * @param actions for each index (denoting a choice set), the number of choices available (denoting
    *                choice indices). order is important.
    * @return the MultiChoice problem assets
    */
  def apply(actions: Array[Int]): MultiChoice = {

    case class Builder(
      mc: MultiChoice = MultiChoice(actions.length, actions, Array.empty, Array.empty),
      currentOffset: Int = 0
    )

    // for each slot, create the BitSet representation of accessing the actions for that slot
    val builder = actions.foldLeft(Builder()) {
      case (builder, choices) =>
        val nextOffset  = builder.currentOffset + choices
        val maskEntries = builder.currentOffset until nextOffset
        val mask        = BitSet(maskEntries: _*)
        val updatedBuilder = builder.copy(
          mc = builder.mc.copy(
            statePartitionMasks = builder.mc.statePartitionMasks :+ mask,
            statePartitionOffsets = builder.mc.statePartitionOffsets :+ builder.currentOffset
          ),
          currentOffset = nextOffset
        )
        updatedBuilder
    }

    builder.mc
  }
}

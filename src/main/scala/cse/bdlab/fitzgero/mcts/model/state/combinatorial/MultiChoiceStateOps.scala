package cse.bdlab.fitzgero.mcts.model.state.combinatorial

import scala.annotation.tailrec

object MultiChoiceStateOps {

  implicit class MultiChoiceStateOpsInstance(state: MultiChoiceState) {

    /**
      * return the choice associated with the provided index, if it exists.
      *
      * @param multiChoice parameters of this state space
      * @param index          the index to retrieve
      * @return the choice, or, None if no choice exists
      */
    def getChoice(
      multiChoice: MultiChoice,
      index: Int
    ): Option[Int] = {
      val offset    = multiChoice.statePartitionOffsets(index)
      val mask      = multiChoice.statePartitionMasks(index)
      val choiceSet = state.intersect(mask)
      val result    = choiceSet.headOption.map { _ - offset }
      result
    }

    /**
      * get all of the choices selected in this MultiChoiceState
      * @param multiChoice parameters of this state space
      * @return all choices made in this state
      */
    def getChoices(multiChoice: MultiChoice): Array[Option[Int]] = {

      val solution: Array[Option[Int]] = Array.fill(multiChoice.slots)(None)

      @tailrec
      def _getChoices(index: Int = 0): Unit = {
        if (index == multiChoice.slots) ()
        else {
          state.getChoice(multiChoice, index) match {
            case None =>
              // shortcut: since we enforce left-to-right assignment, we know that,
              // the first time we find an unassigned slot, we know the remaining
              // slots are unassigned as well
              ()
            case Some(action) =>
              solution.update(index, Some(action))
              _getChoices(index + 1)
          }
        }
      }
      _getChoices()

      solution
    }

    /**
      * adds the provided choice to the state. does not check for previous
      * choices at this index!
      *
      * @param stateOffset    a list of offset values for each partition
      * @param slot index of the choice set within a multi-choice problem
      * @param action action to add within the indexed choice set
      * @return the updated BitSet
      */
    def addChoiceToState(
      stateOffset: StatePartitionOffsets,
      slot: Int,
      action: Int
    ): MultiChoiceState = {
      val offset        = stateOffset(slot)
      val entry         = offset + action
      val updatedBitset = state + entry
      updatedBitset
    }
  }
}

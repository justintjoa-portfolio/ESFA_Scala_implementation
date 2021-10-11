package ESFAArray.MemoryCell.Op

import ESFAArray.MemoryCell.State.MemoryCellState

case class MemoryCellOp() {

  def allocate(state: MemoryCellState, index: Int, value: Int, prev_array_code_and_rank: Option[(Int, Int)]): (Either[String, Int], MemoryCellState) = {
    if (! state.eltDef) {
      state.index = index
      state.value = value
      prev_array_code_and_rank match {
        case Some(prev_code_rank) => {
          val (prev_code, prev_rank) = prev_code_rank
          state.array_code = prev_code + 1
          state.low = prev_code + 1
          state.high = prev_code + 1
          state.rank = prev_rank + 1
        }
        case None => {
          state.array_code = state.handle
          state.low = state.handle
          state.high = state.handle
          state.rank = 1
        }
      }
      state.arrDef = true
      state.eltDef = true
      state.mark = true
      return (Right(state.array_code), state)
    }
    return (Left("Cell is already allocated"), state)
  }

  def deAllocate(state: MemoryCellState): (Either[String, Int], MemoryCellState) = {
    if (state.arrDef) {
      state.arrDef = false
      state.mark = false
      state.rank = 0
      val old_code = state.array_code
      state.array_code = state.handle
      return (Right(old_code), state)
    }
    return (Left("There is no array defined in this cell. Aborting."), state)
  }

  def congrueUp(state: MemoryCellState, code_of_new_entry: Int): MemoryCellState = { // for congruing elements after adding an array entry
    if (state.mark) {
      state.mark = false;
      return state;
    }
    if (state.arrDef) {
      if (state.array_code >= code_of_new_entry) {
        state.array_code += 1
      }
    }
    if (state.eltDef) {
      // explained in section 6.4.1
      if (state.low >= code_of_new_entry) {
        state.low += 1
      }
      if (state.high >= code_of_new_entry) {
        state.high += 1
      }
    }
    return state
  }

  def congrueDown(state: MemoryCellState, code_of_deleted_entry: Int): MemoryCellState = {
    if (state.arrDef) {
      if (state.array_code > code_of_deleted_entry) {
        state.array_code -= 1
      }
    }
    if (state.eltDef) {
      // explained in section 6.6.1
      if (code_of_deleted_entry < state.low) {
        state.low -= 1
        state.high -= 1
      }
      if (state.low <= code_of_deleted_entry && code_of_deleted_entry <= state.high) {
        state.high -= 1
        if (state.high - state.low < 0) {
          state.eltDef = false;
          state.arrDef = false;
        }
      }
    }
    return state
  }
}

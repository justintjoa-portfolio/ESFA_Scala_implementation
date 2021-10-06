package ESFAArray.MemoryCell.Op

import ESFAArray.MemoryCell.State.MemoryCellState

case class MemoryCellOp() {
  private def isEmpty(state: MemoryCellState): Boolean = {
    if (! state.eltDef && ! state.zombie) {
      return true;
    }
    return false;
  }

  def allocate(state: MemoryCellState, index: Int, value: Int, prev_array_code: Option[Int], prev_array_rank: Option[Int]): (Option[Int], MemoryCellState) = {
    var code: Option[Int] = None
    if (isEmpty(state)) {
      state.index = index
      state.value = value
      prev_array_code match {
        case Some(prev_code) => {
          state.array_code = prev_code + 1
          state.low = prev_code + 1
          state.high = prev_code + 1
        }
        case None => {
          state.array_code = state.handle
          state.low = state.handle
          state.high = state.handle
        }
      }
      prev_array_rank match {
        case Some(prev_rank) => {
          state.rank = prev_rank + 1
        }
        case None => {
          state.rank += 1
        }
      }
      state.arrDef = true
      state.eltDef = true
      state.congrue_exempt = true
      code = Some(state.array_code)
    }
    return (code, state)
  }

  def deAllocate(state: MemoryCellState): (Int, MemoryCellState) = {
    state.arrDef = false
    state.mark = false
    state.congrue_exempt = false
    state.rank = 0
    val old_code = state.array_code
    state.array_code = state.handle
    if (state.low - state.high == 0) {
      state.eltDef = false
    }
    return (old_code, state)
  }

  def congrueUp(state: MemoryCellState, code_of_new_entry: Int): MemoryCellState = { // for congruing elements after adding an array entry
    if (state.congrue_exempt) {
      state.congrue_exempt = false;
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

  }
}

package ESFAArray.MemoryCell.Op

import ESFAArray.MemoryCell.State.MemoryCellState

case class MemoryCellOp() {
  private def isEmpty(state: MemoryCellState): Boolean = {
    if (! state.eltDef && ! state.zombie) {
      return true;
    }
    return false;
  }

  def allocate(state: MemoryCellState, index: Int, value: Int, prev_array_code: Option[Int]): (Boolean, MemoryCellState) = {
    var changed = false
    if (isEmpty(state)) {
      state.index = index
      state.value = value
      prev_array_code.foreach(
        (code) => {
          state.array_code = code + 1
          state.low = code + 1
          state.high = code + 1
        }
      )
      state.arrDef = true
      state.eltDef = true
      changed = true
    }
    return (changed, state)
  }

  def congrue(state: MemoryCellState, code_of_new_entry: Int): MemoryCellState = {
    if (state.arrDef) {
      if (state.array_code >= code_of_new_entry) {
        state.array_code += 1
      }
    }
    if (state.eltDef) {
      // explained in section 6.4.1
      if (code_of_new_entry < state.low) {
        state.low += 1
      }
      if (code_of_new_entry <= state.high) {
        state.high += 1
      }
    }
    return state
  }
}

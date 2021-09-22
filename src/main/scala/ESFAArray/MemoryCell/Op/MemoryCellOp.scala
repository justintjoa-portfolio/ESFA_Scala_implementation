package ESFAArray.MemoryCell.Op

import ESFAArray.MemoryCell.State.MemoryCellState

case class MemoryCellOp() {
  def isEmpty(state: MemoryCellState): Boolean = {
    if (! state.eltDef && ! state.zombie) {
      return true;
    }
    return false;
  }

  def allocate(state: MemoryCellState, index: Int, value: Int, prev_array_code: Option[Int]): Option[MemoryCellState] = {
    if (isEmpty(state)) {
      state.index = index
      state.value = value
      prev_array_code.foreach(
        (code) => {
          state.array_code = code + 1
        }
      )
      return Some(state)
    }
    return None
  }

  def withinBounds(state: MemoryCellState, code: Int): Boolean = {
    if (state.arrDef && state.low <= code && code <= state.high) {
      return true;
    }
    return false;
  }

  def congrue(state: MemoryCellState, code_of_new_entry: Int): Unit = {
    if (! withinBounds(state, code_of_new_entry)) {

    }

  }
}

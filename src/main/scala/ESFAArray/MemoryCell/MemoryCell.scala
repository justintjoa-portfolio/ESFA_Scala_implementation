package ESFAArray.MemoryCell

import ESFAArray.MemoryCell.Op.MemoryCellOp
import ESFAArray.MemoryCell.State.MemoryCellState

class MemoryCell(handle: Int) {
  var state: MemoryCellState = MemoryCellState(
    arrDef = false,
    handle,
    0,
    eltDef = false,
    0,
    0,
    0,
    0,
    0,
    notify_flag = false,
    zombie = false,
    select = false,
    mark = false
  )

  def isEmpty(): Boolean = {
    return MemoryCellOp().isEmpty(state)
  }

  def allocate(index: Int, value: Int): Boolean = {

  }
}





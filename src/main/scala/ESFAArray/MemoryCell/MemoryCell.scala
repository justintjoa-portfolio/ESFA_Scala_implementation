package ESFAArray.MemoryCell

import ESFAArray.MemoryCell.Op.MemoryCellOp
import ESFAArray.MemoryCell.State.MemoryCellState

import scala.Option.option2Iterable
import scala.collection.IterableOnce.iterableOnceExtensionMethods

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
    mark = false,
    congrue_exempt = false
  )

  def allocate(index: Int, value: Int, prev_array_code: Int): Option[Int] = {
    val (new_code, new_state) = MemoryCellOp().allocate(state, index, value, prev_array_code)
    state = new_state
    return new_code
  }

  def congrue(code_of_new_entry: Int): Unit = {
    state = MemoryCellOp().congrue(state, code_of_new_entry)
  }
}





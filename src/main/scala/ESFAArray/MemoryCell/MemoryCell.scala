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

  def allocate(index: Int, value: Int, prev_array_code: Option[Int], prev_array_rank: Option[Int]): Option[Int] = {
    val (new_code, new_state) = MemoryCellOp().allocate(state, index, value, prev_array_code, prev_array_rank)
    state = new_state
    return new_code
  }

  def deAllocate(handle: Int): Int = {
    val (code, new_state) = MemoryCellOp().deAllocate(state)
    state = new_state
    return code
  }

  def congrueUp(code_of_new_entry: Int): Unit = {
    state = MemoryCellOp().congrueUp(state, code_of_new_entry)
  }
}





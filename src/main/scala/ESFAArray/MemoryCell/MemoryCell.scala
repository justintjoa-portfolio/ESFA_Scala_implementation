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
    mark = false
  )

  def allocate(index: Int, value: Int, prev_array_code_and_rank: Option[(Int, Int)]): Either[String, Int] = {
    val (code, new_state) = MemoryCellOp().allocate(state, index, value, prev_array_code_and_rank)
    state = new_state
    return code
  }

  def deAllocate(handle: Int): Either[String, Int]= {
    val (code, new_state) = MemoryCellOp().deAllocate(state)
    state = new_state
    return code
  }

  def congrueUp(code_of_new_entry: Int): Unit = {
    state = MemoryCellOp().congrueUp(state, code_of_new_entry)
  }

  def congrueDown(code_of_deleted_entry: Int): Unit = {
    state = MemoryCellOp().congrueDown(state, code_of_deleted_entry)
  }
}





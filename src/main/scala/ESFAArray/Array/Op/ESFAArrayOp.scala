package ESFAArray.Array.Op
import ESFAArray.Array.State.ESFAArrayState

import scala.annotation.tailrec
import scala.math.Ordered.orderingToOrdered

case class ESFAArrayOp {
  val maxHandle = 99

  def encode(state: ESFAArrayState, handle : Int): Option[Int] = {
    val targetCell = state.memoryCellStack(handle)
    if (targetCell.state.arrDef) {
      return Some(targetCell.state.array_code)
    }
    return None
  }

  def update(state: ESFAArrayState, handle: Int, index: Int, value: Int): ESFAArrayState = {
    @tailrec
    def findNextAvailableCell(target_handle: Int, code: Option[Int], index: Int, value: Int): Option[Int] = {
      if (! code.isDefined) {
        return None
      }
      if (target_handle > maxHandle) {
        return None
      }
      var new_code = state.memoryCellStack(target_handle).allocate(index, value, code)
      if (new_code.isDefined) {
        return new_code;
      }
      else {
        var new_target_handle = target_handle + 1
        return findNextAvailableCell(new_target_handle, code, index, value)
      }
    }

    val oldArrayCode = encode(state, handle)
    val new_code = findNextAvailableCell(0, oldArrayCode, index, value)
    new_code match {
      case Some(new_confirmed_code) => {
        state.memoryCellStack.mapInPlace(
          (memoryCell) => {
            memoryCell.congrue(new_confirmed_code)
            memoryCell
          }
        )
        state
      }
      case None => state
    }
  }

  def lookUp(index: Int): Option[Int] = {
    return None
  }

  def delete(array_handle: Int): Option[String] = {
    return true;
  }

  def nextDef(code_of_interest: Int, prev_rank: Int): Option[Int] = {

  }
}

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

  def update(state: ESFAArrayState, handle: Int, index: Int, value: Int): (ESFAArrayState, Option[String]) = {
    @tailrec
    def findNextAvailableCell(state: ESFAArrayState, target_handle: Int, code: Option[Int], index: Int, value: Int): Boolean = {
      if (state.memoryCellStack(target_handle).allocate(index, value, code)) {
        return true;
      }
      else {
        var new_target_handle = target_handle + 1
        return findNextAvailableCell(state, new_target_handle, code, index, value)
      }
    }


    encode(state, handle).map(
      (oldArrayCode) =>

    )
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

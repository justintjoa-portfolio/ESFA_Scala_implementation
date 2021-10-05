package ESFAArray.Array.Op
import ESFAArray.Array.State.ESFAArrayState

import scala.annotation.tailrec
import scala.math.Ordered.orderingToOrdered

case class ESFAArrayOp {
  val maxHandle = 99

  def encode(state: ESFAArrayState, handle : Option[Int]): Option[Int] = {
    handle match {
      case Some(requested_handle) => {
        val targetCell = state.memoryCellStack(requested_handle)
        if (targetCell.state.arrDef) {
          return Some(targetCell.state.array_code)
        }
        return None
      }
      case None => return None
    }
  }

  def update(state: ESFAArrayState, handle: Option[Int], index: Int, value: Int): (Boolean, ESFAArrayState) = {
    @tailrec
    def findNextAvailableCell(target_handle: Int, code: Int, index: Int, value: Int): Option[Int] = {
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
    var new_code: Option[Int] = None
    oldArrayCode match {
      case Some(old_code) => {
        new_code = findNextAvailableCell(0, old_code, index, value)
      }
      case None => return (false, state)
    }

    new_code match {
      case Some(new_confirmed_code) => {
        // "Congruing" in all other cells is performed by tmap in actual implementation
        state.memoryCellStack.mapInPlace(
          (memoryCell) => {
            memoryCell.congrue(new_confirmed_code)
            memoryCell
          }
        )
        (true, state)
      }
      case None => (false, state)
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

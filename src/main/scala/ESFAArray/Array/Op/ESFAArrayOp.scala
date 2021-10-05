package ESFAArray.Array.Op
import ESFAArray.Array.State.ESFAArrayState
import ESFAArray.MemoryCell.MemoryCell

import scala.annotation.tailrec
import scala.math.Ordered.orderingToOrdered

case class ESFAArrayOp {
  val maxHandle = 99

  def encode(state: ESFAArrayState, handle: Int): Option[Int] = {
    if (handle >= maxHandle) {
      return None
    }
    val targetCell = state.memoryCellStack(handle)
    if (targetCell.state.arrDef) {
      return Some(targetCell.state.array_code)
    }
    return None
  }




  def update(state: ESFAArrayState, handle: Option[Int], index: Int, value: Int): (Boolean, ESFAArrayState) = {
    @tailrec
    def findNextAvailableCell(target_handle: Int, code: Option[Int], index: Int, value: Int): Option[Int] = {
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

    var new_code: Option[Int] = None
    handle match {
      case Some(target_handle) => {
        val oldArrayCode = encode(state, target_handle)
        oldArrayCode match {
          case Some(old_code) => {
            new_code = findNextAvailableCell(0, Some(old_code), index, value)
          }
          case None => {}
        }
      }
      case None => {
        new_code = findNextAvailableCell(0, None, index, value)
      }
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
        return (true, state)
      }
      case None => return (false, state)
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

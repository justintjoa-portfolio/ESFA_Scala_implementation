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


  // Return the handle as we aren't doing a system tracking of legible handles
  def update(state: ESFAArrayState, handle: Option[Int], index: Int, value: Int): (Boolean, ESFAArrayState, Option[Int]) = {
    @tailrec
    def findNextAvailableCell(target_handle: Int, code: Option[Int], index: Int, value: Int): (Option[Int], Option[Int]) = {
      if (target_handle > maxHandle) {
        return (None, None)
      }
      var new_code = state.memoryCellStack(target_handle).allocate(index, value, code)
      if (new_code.isDefined) {
        return (Some(target_handle), new_code);
      }
      else {
        var new_target_handle = target_handle + 1
        return findNextAvailableCell(new_target_handle, code, index, value)
      }
    }

    var new_code: Option[Int] = None
    var new_handle: Option[Int] = None
    handle match {
      case Some(target_handle) => {
        val oldArrayCode = encode(state, target_handle)
        oldArrayCode match {
          case Some(old_code) => {
            val (n_handle, n_code) = findNextAvailableCell(0, Some(old_code), index, value)
            new_code = n_code
            new_handle = n_handle
          }
          case None => {
            return (false, state, None)
          }
        }
      }
      case None => {
        val (n_handle, n_code) = findNextAvailableCell(0, None, index, value)
        new_code = n_code
        new_handle = n_handle
      }
    }

    new_code match {
      case Some(new_confirmed_code) => {
        // "Congruing" in all other cells is performed by tmap in actual implementation
        state.memoryCellStack.mapInPlace(
          (memoryCell) => {
            memoryCell.congrueUp(new_confirmed_code)
            memoryCell
          }
        )
        return (true, state, new_handle)
      }
      case None => return (false, state, None)
    }
  }

    def lookUp(state: ESFAArrayState, array_handle: Int, index: Int): Option[Int] = {
      val code = encode(state, array_handle)
      code match {
        case Some(array_code) => {
          state.memoryCellStack.mapInPlace(
            (memoryCell) => {
              if ((memoryCell.state.index == index) && (array_code >= memoryCell.state.low) && (array_code <= memoryCell.state.high)) {
                memoryCell.state.mark = true;
              }
              memoryCell
            }
          )
        }
        case None => {
          return None
        }
      }
      var result: Option[Int] = None
      var highest_rank = 0
      state.memoryCellStack.mapInPlace(
        (memoryCell) => {
          if (memoryCell.state.mark) {
            if (memoryCell.state.rank > highest_rank) {
              highest_rank = memoryCell.state.rank
              result = Some(memoryCell.state.value)
            }
            memoryCell.state.mark = false;
          }
          memoryCell
        }
      )
      return result
    }

    def delete(state: ESFAArrayState, array_handle: Int): (Boolean, ESFAArrayState) = {
      if (array_handle > maxHandle) {
        return (false, state)
      }
      val deleted_array_code = state.memoryCellStack(array_handle).deAllocate(array_handle)
      deleted_array_code match {
        case Some(deleted_array) => {
          state.memoryCellStack.mapInPlace(
            (memoryCell) => {
              memoryCell.congrueDown(deleted_array)
              memoryCell
            }
          )
          return (true, state)
        }
        case None => {
          return (false, state)
        }
      }
    }


    def nextDef(state: ESFAArrayState, array_handle: Int, prev_index: Int): Option[(Int, Int, Int)] = {
      var lowest_next_index: Option[Int] = None
      var next_subarray: Option[(Int, Int, Int)] = None // contains subarray's corresponding handle, index, and value

      encode(state, array_handle) match {
        case Some(code) => {
          state.memoryCellStack.foreach(
            (memoryCell) => {
              if ((code >= memoryCell.state.low) && (code <= memoryCell.state.high) && (memoryCell.state.eltDef)) {
                if (memoryCell.state.index > prev_index) {
                  val potential_index = memoryCell.state.index
                  lowest_next_index match {
                    case Some(lowest_index) => {
                      if (lowest_index > potential_index) {
                        lowest_next_index = Some(potential_index)
                        next_subarray = Some(memoryCell.state.handle, memoryCell.state.index, memoryCell.state.value)
                      }
                    }
                    case None => {
                      lowest_next_index = Some(potential_index)
                      next_subarray = Some(memoryCell.state.handle, memoryCell.state.index, memoryCell.state.value)
                    }
                  }
                }
              }
            }
          )
          next_subarray
        }
        case None => {
          return None
        }
      }
    }

    def prevDef(state: ESFAArrayState, array_handle: Int, anterior_index: Int): Option[(Int, Int, Int)] = {
      var lowest_prev_index: Option[Int] = None
      var prev_subarray: Option[(Int, Int, Int)] = None // contains subarray's corresponding handle, index, and value

      encode(state, array_handle) match {
        case Some(code) => {
          state.memoryCellStack.foreach(
            (memoryCell) => {
              if ((code >= memoryCell.state.low) && (code <= memoryCell.state.high) && (memoryCell.state.eltDef)) {
                if (memoryCell.state.index < anterior_index) {
                  val potential_index = memoryCell.state.index
                  lowest_prev_index match {
                    case Some(lowest_index) => {
                      if (lowest_index > potential_index) {
                        lowest_prev_index = Some(potential_index)
                        prev_subarray = Some(memoryCell.state.handle, memoryCell.state.index, memoryCell.state.value)
                      }
                    }
                    case None => {
                      lowest_prev_index = Some(potential_index)
                      prev_subarray = Some(memoryCell.state.handle, memoryCell.state.index, memoryCell.state.value)
                    }
                  }
                }
              }
            }
          )
          prev_subarray
        }
        case None => {
          return None
        }
      }
    }

    def minDef(state: ESFAArrayState, array_handle: Int): Option[(Int, Int, Int)] = {
      var lowest_defined_index: Option[Int] = None
      var min_defined_subarray: Option[(Int, Int, Int)] = None // contains subarray's corresponding handle and code

      encode(state, array_handle) match {
        case Some(code) => {
          state.memoryCellStack.foreach(
            (memoryCell) => {
              if ((code >= memoryCell.state.low) && (code <= memoryCell.state.high) && (memoryCell.state.eltDef)) {
                val potential_lowest_index = memoryCell.state.index
                lowest_defined_index match {
                  case Some(lowest_index) => {
                    if (lowest_index > potential_lowest_index) {
                      lowest_defined_index = Some(potential_lowest_index)
                      min_defined_subarray = Some(memoryCell.state.handle, memoryCell.state.index, memoryCell.state.value)
                    }
                  }
                  case None => {
                    lowest_defined_index = Some(potential_lowest_index)
                    min_defined_subarray = Some(memoryCell.state.handle, memoryCell.state.index, memoryCell.state.value)
                  }
                }
              }
            }
          )
          min_defined_subarray
        }
        case None => {
          return None
        }
      }
    }

  def maxDef(state: ESFAArrayState, array_handle: Int): Option[(Int, Int, Int)] = {
    var highest_defined_index: Option[Int] = None
    var highest_defined_subarray: Option[(Int, Int, Int)] = None // contains subarray's corresponding handle and code

    encode(state, array_handle) match {
      case Some(code) => {
        state.memoryCellStack.foreach(
          (memoryCell) => {
            if ((code >= memoryCell.state.low) && (code <= memoryCell.state.high) && (memoryCell.state.eltDef)) {
              val potential_highest_index = memoryCell.state.index
              highest_defined_index match {
                case Some(lowest_index) => {
                  if (lowest_index < potential_highest_index) {
                    highest_defined_index = Some(potential_highest_index)
                    highest_defined_subarray = Some(memoryCell.state.handle, memoryCell.state.index, memoryCell.state.value)
                  }
                }
                case None => {
                  highest_defined_index = Some(potential_highest_index)
                  highest_defined_subarray = Some(memoryCell.state.handle, memoryCell.state.index, memoryCell.state.value)
                }
              }
            }
          }
        )
        highest_defined_subarray
      }
      case None => {
        return None
      }
    }
  }

  }



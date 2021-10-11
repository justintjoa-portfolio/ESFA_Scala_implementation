package ESFAArray.Array.Op
import ESFAArray.Array.State.ESFAArrayState
import ESFAArray.MemoryCell.MemoryCell

import scala.annotation.tailrec
import scala.math.Ordered.orderingToOrdered

case class ESFAArrayOp() {
  val maxHandle = 99

  def encode(state: ESFAArrayState, handle: Int): Either[String, (Int, Int)] = {
    if (handle >= maxHandle) {
      return Left("This handle goes beyond the Memory Cells allocated. Aborting.")
    }
    val targetCell = state.memoryCellStack(handle)
    if (targetCell.state.arrDef) {
      return Right(targetCell.state.array_code, targetCell.state.rank)
    }
    return Left("There is no array defined in this memory cell.")
  }


  // Return the handle as we aren't doing a system tracking of legible handles
  def update(state: ESFAArrayState, handle: Option[Int], index: Int, value: Int): (ESFAArrayState, Either[String, Int]) = {
    @tailrec
    def findNextAvailableCell(target_handle: Int, code_and_rank: Option[(Int, Int)], index: Int, value: Int): (Either[String, Int]) = {
      if (target_handle > maxHandle) {
        return Left("No memory cells are free to allocate this new element")
      }
      state.memoryCellStack(target_handle).allocate(index, value, code_and_rank) match {
        case Right(new_code) =>
          return Right(target_handle)
        case Left(_) =>
          var new_target_handle = target_handle + 1
          return findNextAvailableCell(new_target_handle, code_and_rank, index, value)
      }
    }


    var code_of_updated_entry: Option[Int] = None
    var new_handle: Int = 0
    var matched_handle: Option[Int] = handle
    matched_handle match {
      case Some(target_handle) => {
        lookUp(state, target_handle, index) match {
          case Right(old_value) => {
            if (old_value == value) {
              return (state, Left("This key and value pair already exists."))
            }
            matched_handle = prevRank(state, target_handle)
          }
          case Left(_) => {}
        }
      }
      case None => {}
    }

    matched_handle match {
      case Some(target_handle) => {
        val oldArrayCodeAndRank = encode(state, target_handle)
        oldArrayCodeAndRank match {
          case Right(code_and_rank) => {
            findNextAvailableCell(0, Some(code_and_rank), index, value) match {
              case Right(handle) => {
                new_handle = handle
                code_of_updated_entry = Some(code_and_rank._1)
              }
              case Left(error_message) => {
                return (state, Left(error_message))
              }
            }
          }
          case Left(error_message) => {
            return (state, Left(error_message))
          }
        }
      }
      case None => {
        findNextAvailableCell(0, None, index, value) match {
          case Right(handle) => {
            new_handle = handle
          }
          case Left(error_message) => {
            return (state, Left(error_message))
          }
        }
      }
    }

    // "Congruing" in all other cells is performed by tmap in actual implementation
    state.memoryCellStack.mapInPlace(
      (memoryCell) => {
        memoryCell.congrueUp(code_of_updated_entry)
        memoryCell
      }
    )
    return (state, Right(new_handle))
  }

    def lookUp(state: ESFAArrayState, array_handle: Int, index: Int): Either[String, Int] = {
      val code_and_rank = encode(state, array_handle)
      code_and_rank match {
        case Right(array_code_and_rank) => {
          val (array_code, _) = array_code_and_rank
          state.memoryCellStack.mapInPlace(
            (memoryCell) => {
              if ((memoryCell.state.index == index) && (array_code >= memoryCell.state.low) && (array_code <= memoryCell.state.high)) {
                memoryCell.state.mark = true;
              }
              memoryCell
            }
          )
        }
        case Left(error_message) => {
          return Left(error_message)
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
      result match {
        case Some(value) => {
          return Right(value)
        }
        case None =>
          return Left("Element is not defined")
      }
    }

    def delete(state: ESFAArrayState, array_handle: Int): (Option[String], ESFAArrayState) = {
      if (array_handle > maxHandle) {
        return (Some("Array handle is beyond the allocated memory cells. Aborting"), state)
      }
      val deleted_array_code = state.memoryCellStack(array_handle).deAllocate(array_handle)
      deleted_array_code match {
        case Right(deleted_array) => {
          state.memoryCellStack.mapInPlace(
            (memoryCell) => {
              memoryCell.congrueDown(deleted_array)
              memoryCell
            }
          )
          return (None, state)
        }
        case Left(error_message) => {
          return (Some(error_message), state)
        }
      }
    }


    def nextDef(state: ESFAArrayState, array_handle: Int, prev_index: Int): Either[String, (Int, Int, Int)] = {
      var lowest_next_index: Option[Int] = None
      var next_subarray: Option[(Int, Int, Int)] = None // contains subarray's corresponding handle, index, and value

      encode(state, array_handle) match {
        case Right(code_and_rank) => {
          val (code, _) = code_and_rank
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
          next_subarray match {
            case Some(sub_array_representation) =>
              return Right(sub_array_representation)
            case None =>
              return Left("The next definition is not defined.")
          }
        }
        case Left(error_message) => {
          return Left(error_message)
        }
      }
    }

    def prevDef(state: ESFAArrayState, array_handle: Int, anterior_index: Int): Either[String, (Int, Int, Int)]  = {
      var highest_prev_index: Option[Int] = None
      var prev_subarray: Option[(Int, Int, Int)] = None // contains subarray's corresponding handle, index, and value

      encode(state, array_handle) match {
        case Right(code_and_rank) => {
          val (code, _) = code_and_rank
          state.memoryCellStack.foreach(
            (memoryCell) => {
              if ((code >= memoryCell.state.low) && (code <= memoryCell.state.high) && (memoryCell.state.eltDef)) {
                if (memoryCell.state.index < anterior_index) {
                  val potential_index = memoryCell.state.index
                  highest_prev_index match {
                    case Some(lowest_index) => {
                      if (potential_index > lowest_index) {
                        highest_prev_index = Some(potential_index)
                        prev_subarray = Some(memoryCell.state.handle, memoryCell.state.index, memoryCell.state.value)
                      }
                    }
                    case None => {
                      highest_prev_index = Some(potential_index)
                      prev_subarray = Some(memoryCell.state.handle, memoryCell.state.index, memoryCell.state.value)
                    }
                  }
                }
              }
            }
          )
          prev_subarray match {
            case Some(sub_array_representation) =>
              return Right(sub_array_representation)
            case None =>
              return Left("The previous definition is not defined.")
          }
        }
        case Left(error_message) => {
          return Left(error_message)
        }
      }
    }

    def minDef(state: ESFAArrayState, array_handle: Int): Either[String, (Int, Int, Int)] = {
      var lowest_defined_index: Option[Int] = None
      var min_defined_subarray: Option[(Int, Int, Int)] = None // contains subarray's corresponding handle, index, and value

      encode(state, array_handle) match {
        case Right(code_and_rank) => {
          val (code, rank) = code_and_rank
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
          min_defined_subarray match {
            case Some(sub_array_representation) =>
              return Right(sub_array_representation)
            case None =>
              return Left("The minimum definition is not defined.")
          }
        }
        case Left(error_message) => {
          return Left(error_message)
        }
      }
    }

  def maxDef(state: ESFAArrayState, array_handle: Int): Either[String, (Int, Int, Int)] = {
    var highest_defined_index: Option[Int] = None
    var highest_defined_subarray: Option[(Int, Int, Int)] = None // contains subarray's corresponding handle, index, and value

    encode(state, array_handle) match {
      case Right(code_and_rank) => {
        val (code, _) = code_and_rank
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
        highest_defined_subarray match {
          case Some(sub_array_representation) =>
            return Right(sub_array_representation)
          case None =>
            return Left("The maximum definition is not defined.")
        }
      }
      case Left(error_message) => {
        return Left(error_message)
      }
    }
  }

  def prevRank(state: ESFAArrayState, array_handle: Int): Option[Int] = {
    var code_and_rank = encode(state, array_handle)
    code_and_rank match {
      case Right(code_rank) => {
        val (code, rank) = code_rank
        state.memoryCellStack.mapInPlace(
          (memoryCell) => {
            if (memoryCell.state.low <= code && memoryCell.state.high >= code && memoryCell.state.arrDef) {
              memoryCell.state.mark = true
            }
            memoryCell
          }
        )
        var highest_prev_rank: Option[Int] = None
        var handle: Option[Int] = None
        state.memoryCellStack.mapInPlace(
          (memoryCell) => {
            if (memoryCell.state.mark) {
              if (memoryCell.state.rank < rank) {
                highest_prev_rank match {
                  case (Some(highest_tracked_rank)) => {
                    if (highest_tracked_rank < memoryCell.state.rank) {
                      highest_prev_rank = Some(memoryCell.state.rank)
                      handle = Some(memoryCell.state.handle)
                      print("set rank\n")
                    }
                  }
                  case None => {
                    highest_prev_rank = Some(memoryCell.state.rank)
                    handle = Some(memoryCell.state.handle)
                    print("set rank new\n")
                  }
                }
              }
              memoryCell.state.mark = false
            }
            memoryCell
          }
        )
        return handle
      }
      case Left(_) => {
        return None
      }
    }
  }

  }



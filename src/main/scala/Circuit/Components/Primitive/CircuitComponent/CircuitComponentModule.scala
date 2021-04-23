package Circuit.Components.Primitive.CircuitComponent

import Circuit.Components.Primitive.CircuitComponent.State.CircuitComponentState
import Circuit.Components.Primitive.MemoryCell.MemoryCell

case class CircuitComponentModule() {

  def checkEligibility(state:CircuitComponentState, index: Int, code:Int): CircuitComponentState = {
    if ((index == state.memoryCell.index) &&
      (code >= state.memoryCell.low) &&
      (code <= state.memoryCell.high)) {
      state.memoryCell.mark = true
    }
    else {
      state.memoryCell.mark = false
    }
    return state
  }


  def availableHandle(state:CircuitComponentState): Option[Int] = {
    if (! state.memoryCell.eltDef && ! state.memoryCell.arrDef || ! state.memoryCell.zombie) {
      return Some(state.memoryCell.array_handle)
    }
    else {
      return None
    }
  }


  def compareRanks(state:CircuitComponentState, reference:Int, competingRank: Int, competingReference:Int): CircuitComponentState = {
    if (state.memoryCell.mark && (competingRank > state.memoryCell.rank)) {
      return CircuitComponentState(
        state.memoryCell,
        state.inputPort,
        state.inputReference,
        competingRank,
        competingReference
      )
    }
      else {
        return CircuitComponentState(
          state.memoryCell,
          state.inputPort,
          state.inputReference,
          state.memoryCell.rank,
          reference
        )
      }

  }


  def deleteCell(state:CircuitComponentState, code: Int): CircuitComponentState = {
    if ((state.memoryCell.low <= code) && (code <= state.memoryCell.high)) {
      if ((state.memoryCell.low == code) && (code == state.memoryCell.high)) {
        state.memoryCell.arrDef = false
        state.memoryCell.eltDef = false
      }
      else {
        state.memoryCell.arrDef = false
      }
    }
    return state
  }



  def allocate(state:CircuitComponentState, code:Int, index:Int, value:Int): CircuitComponentState = {
    if (! state.memoryCell.arrDef) {
      state.memoryCell.arrDef = true;
      state.memoryCell.eltDef = true;
      state.memoryCell.array_code = code;
      state.memoryCell.low = code;
      state.memoryCell.high = code;
      state.memoryCell.index = index;
      state.memoryCell.value = value;;
    }
    return state

  }


  def getCode(state:CircuitComponentState, handle:Int): Option[(Int, Int)] = {
    if (state.memoryCell.arrDef && (state.memoryCell.array_handle == handle)) {
      return Some(state.memoryCell.array_code, state.memoryCell.rank)
    }
    return None;
  }






}

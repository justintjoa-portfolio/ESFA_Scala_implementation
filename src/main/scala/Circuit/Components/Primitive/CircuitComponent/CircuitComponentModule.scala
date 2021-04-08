package Circuit.Components.Primitive.CircuitComponent

import Circuit.Components.Primitive.MemoryCell.MemoryCell

case class CircuitComponentModule() {

  def checkEligibility(index: Int, code:Int, memoryCell:MemoryCell, action: () => Unit): () => Unit = {
    if ((index == memoryCell.index) &&
      (code >= memoryCell.low) &&
      (code <= memoryCell.high)) {
      action
    }
    return  () => Unit
  }

  def compareRanks(mark:Boolean, rank1:Int, rank2:Int, outReference1:Int, outReference2:Int ): (Int, Int) = {
    if (mark) {
      if (rank2 > rank1) {
        return (rank2, outReference2)
      }
      else {
        return (rank1, outReference1)
      }
    }
    return (rank1, outReference1)
  }

  def deleteCell(code: Int, memoryCell: MemoryCell, actionRemove: () => Unit, actionUnassociate: () => Unit): () => Unit = {
    if ((memoryCell.low <= code) && (code <= memoryCell.high)) {
      if ((memoryCell.low == code) && (code == memoryCell.high)) {
        return actionRemove
      }
      else {
        return actionUnassociate
      }
    }
    else {
      () => Unit
    }
  }

  def getCode(memoryCell:MemoryCell, handle:Int): Option[(Int, Int)] = {
    if (memoryCell.arrDef && (memoryCell.array_handle == handle)) {
      return Some(memoryCell.array_code, memoryCell.rank)
    }
    return None;
  }


  def allocate(arrDef:Boolean, addArray: () => Unit): () => Unit = {
    if (arrDef) {
      return () => print("Array already exists here.");
    }
    else {
      return addArray
    }

  }



}

package Circuit.Components.Primitive

class CircuitComponent(private var _memoryCell: MemoryCell)  {

  var inputPort: Int = 0

  var inputReference: Int = 0

  var outputPort: Int = 0

  var outputReference: Int = 0

  //pure
  def checkEligibility(index: Int, code:Int, currentIndex: Int,
                       low: Int, high: Int): Option[(MemoryCell) => Unit] = {
    if ((index == currentIndex) &&
      (code >= low) &&
      (code <= high)) {
        return Some(
          (memoryCell: MemoryCell) =>
            memoryCell.mark = true
        )
    }
    return None

  }

  def compareRanksR(mark:Boolean, rank1:Int, rank2:Int, outReference1:Int, outReference2:Int ): (Int, Int) = {
    if (mark) {
      if (rank1 > rank2) {
        return (rank1, outReference1)
      }
      else {
        return (rank2, outReference2)
      }
    }
    return (rank1, outReference1)
  }

  def deleteCellR(code: Int, low:Int, high:Int): Option[(MemoryCell) => Unit] = {
    if ((low <= code) && (code <= high)) {
      if ((low == code) && (code == high)) {
        return Some(
          (memoryCell:MemoryCell) =>
            memoryCell.arrDef = false
        )
      }
      else {
        return Some(
          (memoryCell:MemoryCell) => {
            memoryCell.arrDef = false;
            memoryCell.eltDef = false;
          }
        )
      }
    }
    else {
      None
    }
  }





  //impure
  def markIfEligible(index: Int, code:Int): Unit = {
    checkEligibility(index, code, _memoryCell.index, _memoryCell.low, _memoryCell.high).foreach(
      (function) => function(_memoryCell)
    )
  }

  def availableHandler:Boolean = {
    return (_memoryCell.zombie || _memoryCell.eltDef);
  }

  def compareRanks(): Unit = {
    val results = compareRanksR(_memoryCell.mark, inputPort, _memoryCell.rank, inputReference, _memoryCell.reference)
    outputPort = results._1
    outputReference = results._2
  }

  def deleteCell(code:Int): Unit = {
    deleteCellR(code, _memoryCell.low, _memoryCell.high).foreach(
      (function) => function(_memoryCell)
    )

  }


}

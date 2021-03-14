package Circuit.Components.Primitive

class CircuitComponent(private var _memoryCell: MemoryCell)  {

  var inputPort: Int = 0

  var outputPort: Int = 0

  //pure
  def checkEligibility(index: Int, code:Int, currentIndex: Int,
                       low: Int, high: Int): Option[(MemoryCell) => Unit] = {
    if ((index == currentIndex) &&
      (code > low) &&
      (code < high)) {
        return Some(
          (memoryCell: MemoryCell) =>
            memoryCell.mark = true
        )
    }
    return None

  }

  def compareRanksR(mark:Boolean, rank1:Int, rank2:Int): Int = {
    if (mark) {
      return rank1.max(rank2)
    }
    return rank1
  }




  //impure
  def markIfEligible(index: Int, code:Int): Unit = {
    checkEligibility(index, code, _memoryCell.index, _memoryCell.low, _memoryCell.high).map(
      (function) => function(_memoryCell)
    )
  }

  def availableHandler:Boolean = {
    return (_memoryCell.zombie || _memoryCell.eltDef);
  }

  def compareRanks(): Unit = {
    outputPort = compareRanksR(_memoryCell.mark, inputPort, _memoryCell.rank)
  }


}

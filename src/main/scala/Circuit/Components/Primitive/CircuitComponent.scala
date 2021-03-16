package Circuit.Components.Primitive

class CircuitComponent(private var _memoryCell: MemoryCell)  {

  var inputPort: Int = 0

  var inputReference: Int = 0

  var outputPort: Int = 0

  var outputReference: Int = 0

  //pure
  def checkEligibility(index: Int, code:Int, memoryCell:MemoryCell): Option[() => Unit] = {
    if ((index == memoryCell.index) &&
      (code >= memoryCell.low) &&
      (code <= memoryCell.high)) {
        return Some(
          () =>
            _memoryCell.mark = true
        )
    }
    return None

  }

  def compareRanksR(mark:Boolean, rank1:Int, rank2:Int, outReference1:Int, outReference2:Int ): (Int, Int) = {
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

  def deleteCellR(code: Int, memoryCell: MemoryCell): Option[() => Unit] = {
    if ((memoryCell.low <= code) && (code <= memoryCell.high)) {
      if ((memoryCell.low == code) && (code == memoryCell.high)) {
        return Some(
          () => {
            _memoryCell.arrDef = false
            _memoryCell.eltDef = false;
          }
        )
      }
      else {
        return Some(
          () =>
            _memoryCell.arrDef = false
        )
      }
    }
    else {
      None
    }
  }

  def getCodeR(memoryCell:MemoryCell, handle:Int): Option[(Int, Int)] = {
    if (memoryCell.arrDef && (_memoryCell.array_handle == handle)) {
      return Some(_memoryCell.array_code, _memoryCell.rank)
    }
    return None;
  }


  def allocateR(arrDef:Boolean, code:Int, index:Int, value:Int): (Either[String, Nothing], Option[() => Unit]) = {
    if (arrDef) {
      return (Left("Array already exists here"), None);
    }
    else {
      return (Right(), Some(
        () => {
          _memoryCell.arrDef = true;
          _memoryCell.eltDef = true;
          _memoryCell.array_code = code;
          _memoryCell.low = code;
          _memoryCell.high = code;
          _memoryCell.index = index;
          _memoryCell.value = value;
        }
      ))
    }

  }




  //impure
  def markIfEligible(index: Int, code:Int): Unit = {
    checkEligibility(index, code, _memoryCell).foreach(
      (function) => function()
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
    deleteCellR(code, _memoryCell).foreach(
      (function) => function()
    )

  }

  def getCode(handle:Int): Option[(Int, Int)] = {
    return getCodeR(_memoryCell, handle);
  }

  def allocate(code:Int, index:Int, value:Int): Either[String, String] = {
    val result = allocateR(_memoryCell.arrDef, code, index, value)
    result._2.foreach(
      (function) => function()
    )
    return result._1
  }


}

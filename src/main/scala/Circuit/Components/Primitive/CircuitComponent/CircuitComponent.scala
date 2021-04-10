package Circuit.Components.Primitive.CircuitComponent

import Circuit.Components.Primitive.MemoryCell.MemoryCell

class CircuitComponent(var _memoryCell: MemoryCell)  {

  var inputPort: Int = 0

  var inputReference: Int = 0

  var outputPort: Int = 0

  var outputReference: Int = 0


  def markIfEligible(index: Int, code:Int): Unit = {
    CircuitComponentModule().checkEligibility(index, code, _memoryCell, () =>
      _memoryCell.mark = true)()
  }

  def availableHandle(): Option[Int] = {
    if (! _memoryCell.eltDef && ! _memoryCell.arrDef || ! _memoryCell.zombie) {
      return Some(_memoryCell.array_handle)
    }
    else {
      return None
    }
  }



  def compareRanks(): Unit = {
    val results = CircuitComponentModule().compareRanks(_memoryCell.mark, inputPort, _memoryCell.rank, inputReference, _memoryCell.reference)
    outputPort = results._1
    outputReference = results._2
  }

  def deleteCell(code:Int): Unit = {
    CircuitComponentModule().deleteCell(code, _memoryCell, () => {
      _memoryCell.arrDef = false
      _memoryCell.eltDef = false;
    }, () =>
      _memoryCell.arrDef = false)()

  }


  def initCell(identifier: Int): Unit = {

  }

  def allocate(code:Int, index:Int, value:Int): Unit = {
    CircuitComponentModule().allocate(_memoryCell.arrDef,
      () => {
        _memoryCell.arrDef = true;
        _memoryCell.eltDef = true;
        _memoryCell.array_code = code;
        _memoryCell.low = code;
        _memoryCell.high = code;
        _memoryCell.index = index;
        _memoryCell.value = value;
      })()
  }


}

package ESFAArray.Array.State

import ESFAArray.MemoryCell.MemoryCell

case class ESFAArrayState() {
  var memoryCellStack: Array[MemoryCell] = {
    var memoryCellStackOutput: Array[MemoryCell] = new Array[MemoryCell](100)
    for (x <- 0 to 99) {
      var addition = (new MemoryCell(x))
      memoryCellStackOutput(x) = addition
    }
    memoryCellStackOutput
  }
}

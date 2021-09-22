package ESFAArray.Array.State

import ESFAArray.MemoryCell.MemoryCell

case class ESFAArrayState() {
  var memoryCellStack = {
    var memoryCellStack: Array[MemoryCell] = Array[MemoryCell]()
    for (x <- 0 to 99) {
      memoryCellStack :+ (new MemoryCell(x))
    }
    memoryCellStack
  }
}

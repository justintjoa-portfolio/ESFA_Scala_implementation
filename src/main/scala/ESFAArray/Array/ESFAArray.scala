package ESFAArray
import MemoryCell.MemoryCell

//let's assume array only carries ints for now
case class ESFAArray() {
  val maxHandle = 99

  var availableHandle = 0

  private var memoryCellStack = {
    var memoryCellStack: Array[MemoryCell] = Array[MemoryCell]()
    for (x <- 0 to 99) {
      memoryCellStack :+ (new MemoryCell(x))
    }
    memoryCellStack
  }

  def encode(handle: Int): Option[Int] = {
    val targetCell = memoryCellStack(handle)
    if (targetCell.state.arrDef) {
      return Some(targetCell.state.array_code)
    }
    return None
  }

  def update(index: Int, value: Int): Option[String] = {

  }

  def lookUp(index: Int): Option[Int] = {
    return None
  }

  def delete(array_handle: Int): Option[String] = {
    return true;
  }

  def nextDef(code_of_interest: Int, prev_rank: Int): Option[Int] = {

  }

}

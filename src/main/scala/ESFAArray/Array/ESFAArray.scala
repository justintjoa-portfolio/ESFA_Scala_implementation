package ESFAArray.Array
import State.ESFAArrayState


//let's assume array only carries ints for now
case class ESFAArray() {

  private var state = ESFAArrayState()

  def encode(handle: Int): Option[Int] = {
    val targetCell = state.memoryCellStack(handle)
    if (targetCell.state.arrDef) {
      return Some(targetCell.state.array_code)
    }
    return None
  }

  def update(array_handle: Int, index: Int, value: Int): Option[String] = {

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

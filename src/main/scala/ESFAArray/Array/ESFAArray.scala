package ESFAArray.Array
import State.ESFAArrayState
import Op.ESFAArrayOp

//let's assume array only carries ints for now
case class ESFAArray() {

  private var state = ESFAArrayState()

  def update(array_handle: Int, index: Int, value: Int): Boolean = {
    val (didUpdate, newstate) = ESFAArrayOp().update(state, array_handle, index, value)
    state = newstate
    return didUpdate
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

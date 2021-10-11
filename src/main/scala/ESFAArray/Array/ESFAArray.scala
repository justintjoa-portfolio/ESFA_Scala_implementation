package ESFAArray.Array
import State.ESFAArrayState
import Op.ESFAArrayOp

//let's assume array only carries ints for now
case class ESFAArray() {

  private var state = ESFAArrayState()

  def update(handle: Option[Int], index: Int, value: Int): Either[String, Int] = {
    val (newstate, new_handle) = ESFAArrayOp().update(state, handle, index, value)
    state = newstate
    return new_handle
  }

  def lookUp(handle: Int, index: Int): Either[String, Int] = {
    ESFAArrayOp().lookUp(state, handle, index)
  }

  def delete(array_handle: Int): Option[String] = {
    val (deleteMessage, new_state) = ESFAArrayOp().delete(state, array_handle)
    state = new_state
    return deleteMessage
  }

  def nextDef(array_handle: Int, prev_index: Int): Either[String, (Int, Int, Int)]  = {
    ESFAArrayOp().nextDef(state, array_handle, prev_index)
  }

  def prevDef(array_handle: Int, anterior_index: Int): Either[String, (Int, Int, Int)] = {
    ESFAArrayOp().prevDef(state, array_handle, anterior_index)
  }

  def minDef(array_handle: Int): Either[String, (Int, Int, Int)] = {
    ESFAArrayOp().minDef(state, array_handle)
  }

  def maxDef(array_handle: Int): Either[String, (Int, Int, Int)] = {
    ESFAArrayOp().maxDef(state, array_handle)
  }

}

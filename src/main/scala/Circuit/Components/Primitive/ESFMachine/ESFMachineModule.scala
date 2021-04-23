package Circuit.Components.Primitive.ESFMachine

import Circuit.Components.Primitive.CircuitComponent.CircuitComponent
import scala.math.Ordering.Implicits.infixOrderingOps
import scala.collection.parallel.CollectionConverters.ArrayIsParallelizable
import scala.math.Ordering.Implicits.infixOrderingOps


case class ESFMachineModule() {

  def lookUp(state:ESFMachineState, index: Int): Option[Int] = {
    return None
  }

  def update(state:ESFMachineState, maxHandle:Int, index:Int, value:Int): (Option[String], ESFMachineState) = {
    if (state.highestHandle == maxHandle) {
      return (Some("Error! There is no more space"), state)
    }
    state.esfMachineStack(state.highestHandle+1).allocate(state.highestHandle+1, index, value) //assume there is no
    //variable binding to handles, so for now, assume the code is not the code of an array in imperative code
    //but just the handle of the memory Cell itself, in other word as if it was a brand new array
    state.esfMachineStack.foreach(
      (circuitComponent) => {
        if (!(circuitComponent.expose.memoryCell.array_handle == state.highestHandle)) {
          circuitComponent.includeHandleInSet(state.highestHandle)
        }
      }
    )
    return(None, ESFMachineState(
      state.esfMachineStack,
      state.highestHandle + 1
    ))
  }

  def delete(state:ESFMachineState, identifier:Int): (Option[String], ESFMachineState) = {

  }


}

package Circuit.Components.Primitive.CircuitComponent

import Circuit.Components.Primitive.CircuitComponent.State.CircuitComponentState
import Circuit.Components.Primitive.MemoryCell.MemoryCell

class CircuitComponent(private var state: CircuitComponentState, reference:Int)  {

  val module = CircuitComponentModule()

  def expose(): CircuitComponentState = {
    return state
  }

  def checkEligibility(index: Int, code:Int): Unit = {
    state = module.checkEligibility(state, index, code)
  }



  def compareRanks(reference:Int, competingRank: Int, competingReference:Int): Unit = {
    state = module.compareRanks(state, reference, competingRank, competingReference)

  }


  def deleteCell(code: Int): Unit = {
    state = module.deleteCell(state, code)
  }


  def initCell(identifier: Int): Unit = {

  }

  def allocate(code:Int, index:Int, value:Int): Unit = {
    state = module.allocate(state, code, index, value)

  }


}

package Circuit.Components.Primitive.CircuitComponent

import Circuit.Components.Primitive.CircuitComponent.State.CircuitComponentState
import Circuit.Components.Primitive.MemoryCell.MemoryCell

class CircuitComponent(private val reference:Int)  {

  private var state: CircuitComponentState = CircuitComponentState(
    MemoryCell(reference),
  0,
  0,
  0,
  0
  )


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



  def allocate(code:Int, index:Int, value:Int): Unit = {
    state = module.allocate(state, code, index, value)

  }


}

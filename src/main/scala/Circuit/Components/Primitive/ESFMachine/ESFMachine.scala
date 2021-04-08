package Circuit.Components.Primitive.ESFMachine

import Circuit.Components.Primitive.CircuitComponent.CircuitComponent

class ESFMachine(private var esfMachineArray: Array[CircuitComponent]) {

  var _mappings:Map[Int, Int] = Map()

  val highestHandle:Int = 100

  var lowestHandle: Int = 0

  def updateExistentArray(input: Option[Int], code: Int, index: Int, value: Int): Unit = {
    ESFMachineModule().updateExistentArray(input, () => {
      esfMachineArray(lowestHandle).allocate(code, index, value)
      lowestHandle+=1
    })()
  }

  def update(ifInMap:Boolean, identifier:Int, index:Int, value:Int): Unit = {
    ESFMachineModule().update(
      ifInMap,
      () => {
        esfMachineArray(lowestHandle).allocate(lowestHandle, index, value);
        _mappings + (identifier, lowestHandle)
        lowestHandle+=1
      },
      () => updateExistentArray(lookUp(identifier, index), identifier, index, value)
    )()

  }

  def sweep(): Unit = {

  }

  def update(identifier:Int, index:Int, value:Int): Unit = {


  }

  def lookUp(identifier: Int, index:Int): Option[Int] = {
    return None
  }



}

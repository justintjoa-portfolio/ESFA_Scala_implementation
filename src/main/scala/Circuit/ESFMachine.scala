package Circuit

import Circuit.Components.Primitive.{CircuitComponent, MemoryCell}

class ESFMachine(private var esfMachineArray: Array[CircuitComponent]) {

  var _mappings:Map[Int, Int] = Map()

  val highestHandle:Int = 100

  var lowestHandle: Int = 0


  //pure

  def updateExistentArray(input: Option[Int], code: Int, index: Int, value: Int): Option[() => Unit] = {
    if (input isDefined) {

    }
    else {
      return Some(
        () => {
          esfMachineArray(lowestHandle).allocate(code, index, value)
          lowestHandle+=1
        }
      )
    }
  }

  def updateR(ifInMap:Boolean, identifier:Int, index:Int, value:Int): Option[() => Unit] = {
    if (!ifInMap) {
      return Some(() => {
        esfMachineArray(lowestHandle).allocate(lowestHandle, index, value)
        _mappings + (identifier, lowestHandle)
        lowestHandle+=1
      })
    }
    else {
      return Some(() => {
        updateExistentArray(lookUp(identifier, index), identifier, index, value).foreach(
          function => function()
        )
      })
    }
  }


  //impure

  def sweep(): Unit = {

  }

  def update(identifier:Int, index:Int, value:Int): Unit = {


  }

  def lookUp(identifier: Int, index:Int): Option[Int] = {
    return None
  }



}

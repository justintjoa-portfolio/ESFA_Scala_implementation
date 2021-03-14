package Circuit

import Circuit.Components.Primitive.{CircuitComponent, MemoryCell}

class ESFMachine(private var esfMachineArray: Array[CircuitComponent]) {

  var _mappings:Map[Int, Int] = Map()

  def encodeSR(exists:Boolean, handle:Int): (Option[Int], Int) = {
    if (!exists) {
      return (None, 0)
    }
    //parallel associative search for cell
  }


  def sweep(): Unit = {

  }

  def tFold(): Unit = {

  }

  def tMap(): Unit = {
  }

  def tScanL: Unit = {

  }

  def allocate(index:Int, value:Int): Unit = {

  }



}

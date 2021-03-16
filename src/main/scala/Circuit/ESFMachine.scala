package Circuit

import Circuit.Components.Primitive.{CircuitComponent, MemoryCell}

class ESFMachine(private var esfMachineArray: Array[CircuitComponent]) {



  def encodeSR(exists:Boolean, handle:Int, choose:
  (CircuitComponent, CircuitComponent) => (CircuitComponent)): Option[(Int, Int)] = { //code and rank
    if (!exists) {
      None
    }
    tFold(esfMachineArray, choose)

    //parallel associative search for cell
  }

  def tFold(esfMachineArray: Array[CircuitComponent],
            choose: (CircuitComponent, CircuitComponent) =>
              (CircuitComponent)): Option[(Int, Int)] = {
    //go through the array and use chose function to find the right value
    //if choose selects something, in next "iteration" you can do nothing
    //can also return nothing
    return Some((0, 0)) //stub
  }



  def sweep(): Unit = {

  }


  def tMap(): Unit = {
  }

  def tScanL: Unit = {

  }

  def allocate(index:Int, value:Int): Unit = {

  }



}

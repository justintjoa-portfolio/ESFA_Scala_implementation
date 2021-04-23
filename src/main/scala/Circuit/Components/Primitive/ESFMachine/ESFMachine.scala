package Circuit.Components.Primitive.ESFMachine

import Circuit.Components.Primitive.CircuitComponent.CircuitComponent

class ESFMachine() {



  private var esfMachineState = {
      var esfArray = Array[CircuitComponent](100)
      for (x <- 0 to 99) {
        esfArray(x) = new CircuitComponent(x)
      }
     ESFMachineState(
       esfArray,
       0,
       0
     )

  }


  def expose(): ESFMachineState = {
    return esfMachineState
  }


  def update(index:Int, value:Int): Option[String] = {

  }

  def delete(identifier:Int): Option[String] = {
    return true;
  }




}

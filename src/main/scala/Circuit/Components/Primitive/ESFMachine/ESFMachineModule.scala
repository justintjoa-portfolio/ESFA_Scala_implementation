package Circuit.Components.Primitive.ESFMachine

import Circuit.Components.Primitive.CircuitComponent.CircuitComponent

import scala.collection.parallel.CollectionConverters.ArrayIsParallelizable


case class ESFMachineModule() {

  def update(targetIdentifier:Option[Int], identifier:Int, index:Int, value:Int): Either[String, Int] = {
    return Right(1)
  }

  def copyArray(identifier: Int, newIdentifier: Int): Boolean = {
    return true;
  }

  def lookUp(inputMap: Map[Int, Int], esfMachineArray: Array[CircuitComponent], identifier: Int, index:Int): Option[Int] = {
    //for all elements in esfMachineArray, set mark = true for elements where
    //low <= code <= high
    //then, in the second sweep, for all elements that mark = true
    //get element with highest rank and return its value
    if (inputMap.contains(identifier)) {
      inputMap.get(identifier).map(
        (index) => {
          //reference ending of section 6.5 of paper for algorithm
          val code = esfMachineArray(index)._memoryCell.array_code
          esfMachineArray.map(
            (esfMachine) => esfMachine.markIfEligible(index, code) //sweep 1
          ).par.fold(0)((component1: CircuitComponent, component2: CircuitComponent) => {
            component1._memoryCell.rank.max(component2._memoryCell.rank)
          })
        }
      ).orElse(
        return None
      )
    }
    else {
      return None
    };
  }

  def initArray(inputMap: Map[Int, Int], identifier: Int, action: () => Unit): Either[String, () => Unit] = {
    if (! inputMap.contains(identifier)) {
      return Right(action)
    }
    else {
      return Left("Array already exists!")
    };
  }

  def delete(inputMap: Map[Int, Int], arrayIdentifier: Int, deleteArray: () => Unit): Either[String, () => Unit] = {
    if (inputMap.contains(arrayIdentifier)) {
      return Right(deleteArray)
    }
    else {
      return Left("Array does not exist!")
    }
  }

}

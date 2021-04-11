package Circuit.Components.Primitive.ESFMachine

import Circuit.Components.Primitive.CircuitComponent.CircuitComponent
import scala.math.Ordering.Implicits.infixOrderingOps
import scala.collection.parallel.CollectionConverters.ArrayIsParallelizable
import scala.math.Ordering.Implicits.infixOrderingOps


case class ESFMachineModule() {

  def update(targetIdentifier:Option[Int], identifier:Int, index:Int, value:Int): Either[String, Int] = {
    return Right(1)
  }

  def copyArray(inputMap: Map[Int, Int], addArray: () => Unit,
                originalIdentifier: Int, arrayIdentifier: Int, highestHandle: Int):
              (Either[String, (Map[Int, Int], () => Unit)]) = {
    if (! inputMap.contains(arrayIdentifier) && inputMap.contains(originalIdentifier)) {
      return Right(inputMap + (arrayIdentifier -> highestHandle), addArray)
    }
    else {
      return Left("Either original array doesn't exist, or target array already exists!")
    };
  }

  def lookUp(inputMap: Map[Int, Int], esfMachineArray: Array[CircuitComponent], identifier: Int, index:Int): Option[Int] = {
    if (inputMap.contains(identifier)) {
      inputMap.get(identifier).map(
        (index) => {
          //reference ending of section 6.5 of paper for algorithm
          val code = esfMachineArray(index)._memoryCell.array_code
          val markedArray = esfMachineArray.map(
            (esfMachine) => {
              esfMachine.markIfEligible(index, code)
              esfMachine
            } //sweep 1
          ).filter(
            _._memoryCell.mark == true
          )
          markedArray.par.fold(markedArray(0))(
            (a, b) => {
              if (a._memoryCell.rank > b._memoryCell.rank) {
                a
              }
              b;
            }
          )._memoryCell.value
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

  def delete(inputMap: Map[Int, Int], deleteFromArray: () => Unit, arrayIdentifier: Int): (Either[String, (Map[Int, Int], () => Unit)]) = {
    if (inputMap.contains(arrayIdentifier)) {
      Right(inputMap.-(arrayIdentifier), deleteFromArray)
    }
    else {
      return Left("Array does not exist!")
    }
  }

}

import Circuit.Components.Primitive.ESFMachine.ESFMachine

//let's assume array only carries ints for now
case class ESFAArray(esfMachine: ESFMachine) {


  def update(targetIdentifier:Option[Int], identifier:Int, index:Int, value:Int): Either[String, Int] = {
    return Right(1)
  }

  def copyArray(identifier: Int, newIdentifier: Int): Boolean = {
    return true;
  }

  def lookUp(identifier: Int, index:Int): Option[Int] = {
    return None
  }

  def initArray(identifier: Int): Boolean = {
    return true;
  }

  def delete(identifier:Int): Boolean = {
    return true;
  }

}






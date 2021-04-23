import Circuit.Components.Primitive.ESFMachine.ESFMachine

//let's assume array only carries ints for now
case class ESFAArray(esfMachine: ESFMachine) {


  def update(index:Int, value:Int): Option[String] = {
    
  }

  def lookUp(index:Int): Option[Int] = {
    return None
  }

  def delete(identifier:Int): Option[String] = {
    return true;
  }

}






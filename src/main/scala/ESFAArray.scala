import Circuit.ESFMachine

//let's assume array only carries ints for now
case class ESFAArray(esfMachine: ESFMachine) {


  def empty(): Option[ESFAArray] = {
    return None
  }

  def update(handle:Int, index:Int, value:Int, isPointerToHeap:Boolean): Unit = {
    print("hello!")
  }

  def lookUp(handle: Int, index:Int): Option[Int] = {
    return None
  }

  def minDef: Option[Int] = {
    return None
  }

  def maxDef: Option[Int] = {
    return None
  }

  def nextDef: Option[Int] = {
    return None
  }

  def prevDef: Option[Int] = {
    return None
  }




 def delete(): Unit = {

 }

  def killZombies(): Unit = {

  }



}

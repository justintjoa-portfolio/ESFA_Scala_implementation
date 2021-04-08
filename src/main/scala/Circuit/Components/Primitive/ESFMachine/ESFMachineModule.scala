package Circuit.Components.Primitive.ESFMachine

case class ESFMachineModule() {

  def updateExistentArray(input: Option[Int], allocate: () => Unit): () => Unit = {
    if (input isDefined) {
      () => Unit
    }
    else {
      return allocate
    }
  }

  def update(ifInMap:Boolean, advance: () => Unit, update: () => Unit): () => Unit = {
    if (!ifInMap) {
      advance
    }
    else {
      update
    }
  }


}

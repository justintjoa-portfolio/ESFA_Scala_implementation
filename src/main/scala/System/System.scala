package System

import Circuit.Components.Primitive.MemoryCell
import Circuit.ESFMachine

import java.util.{Timer, TimerTask}

class System(private var esfMachine:ESFMachine) {



  class Action extends TimerTask {
    def run() {
      hilo = !hilo
    }
  }

  private var hilo:Boolean = true
  def run(): Unit = {
    val timer = new Timer()
    timer.schedule(new Action(), 0, 1000)
  }

}

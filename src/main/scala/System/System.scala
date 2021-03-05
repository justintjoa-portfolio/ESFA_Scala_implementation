package System

import MemoryCell.MemoryCell

import java.util.{Timer, TimerTask}

class System {

  private var sysArray = new Array[MemoryCell](100) //operate via parallel computing collection

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

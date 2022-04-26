package vexriscv.plugin.riscvdebug;

import spinal.core._
import spinal.lib.bus.misc._
import scala.collection.Seq

object DMISlaveFactory {
  def apply(bus: DMI) = new DMISlaveFactory(bus)
}


class DMISlaveFactory(bus: DMI) extends BusSlaveFactoryDelayed{

  def IGNORE_OP = B"00"
  def READ_OP = B"01"
  def WRITE_OP = B"10"
  val askWrite = bus.req.payload.op === WRITE_OP
  val askRead = bus.req.payload.op === READ_OP
  val doWrite = bus.req.valid && bus.req.payload.op === WRITE_OP addAttribute("MARK_DEBUG", "TRUE")
  val doRead = bus.req.ready && bus.req.valid && bus.req.payload.op === READ_OP addAttribute("MARK_DEBUG", "TRUE")

  bus.req.ready := True
  bus.rsp.valid := True
  bus.rsp.payload.data.clearAll() // default assignment
  bus.rsp.payload.op := IGNORE_OP

  override def readAddress() = bus.req.payload.address.asUInt
  override def writeAddress() = bus.req.payload.address.asUInt

  override def readHalt() = bus.rsp.valid := False
  override def writeHalt() = bus.req.ready := False


  override def busDataWidth   = widthOf(bus.req.payload.data)
  override def wordAddressInc = 1


  override def build(): Unit = {
    super.doNonStopWrite(bus.req.payload.data)

    def doMappedElements(jobs: Seq[BusSlaveFactoryElement]) = super.doMappedElements(
      jobs = jobs,
      askWrite = askWrite,
      askRead = askRead,
      doWrite = doWrite,
      doRead = doRead,
      writeData = bus.req.payload.data,
      readData = bus.rsp.payload.data
    )


    switch(bus.req.address) {
      for ((address, jobs) <- elementsPerAddress if address.isInstanceOf[SingleMapping]) {
        is(address.asInstanceOf[SingleMapping].address) {
          doMappedElements(jobs)
        }
      }
    }
  }
}


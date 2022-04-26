package vexriscv.plugin.riscvdebug

import spinal.core._
import spinal.lib._
import spinal.lib.com.jtag.{Jtag}

/* Debug modules are slaves to a bus called the debug module inteface (DMI) the master of the bus is the debug transport
 module(s). the debug module interface can be a trivial bus with one master and one slave.

the DMI uses between 7 and 32 address bits. it supports read and write operations. the bottom of the address space is
used for the first DM extra space can be used for custom debug devices, other...
The debug module is controlled via register accesses to its DMI address space

*/
case class DMI_req(abits: Int) extends Bundle{
  val address = Bits(abits bit)    //< requested address
  val data = Bits(32 bits)  //< requested data
  val op = Bits(2 bits)     //< same meaning as `op` field
}
case class DMI_rsp() extends Bundle{
  val data = Bits(32 bits)  //< response data
  val op = Bits(2 bits)     //< same meaning as `op` field
}
case class DMI(abits: Int) extends Bundle with IMasterSlave {

  val req = Stream(DMI_req(abits)) //< DTM to DM
  val rsp = Stream(DMI_rsp()) //< DM to DTM

  override def asMaster(): Unit = {
    master(req)
    slave(rsp)
  }


  /**
   * connects the DMI bus to a Jtag port trough a Jtag DTM
   * @return jtag port
   */
  def fromJtag(): Jtag = {
    // arguments from DTM jtag defined in riscv-debug-stable
    val dtm = new JtagDTM(abits)(5, 1, 0x10, 0x11)
    this <> dtm.io.dmi
    dtm.io.jtag
  }

  /**
   * connects the DMI bus to 3 BSCANE2 primitives (USER1/2/3)
   * can be used with remapping IR codes:
   * `riscv set_ir idcode 3`
   * `riscv set_ir dtmcs 4`
   * `riscv set_ir dmi 34`
   * Check the BDSL file of your part to get the IR codes for USERx BSCAN
   */
  def fromBscane2() {
  val dtm = new BscaneDTM(abits)
  this <> dtm.io.dmi
  }

  /**
   * connects the DMI bus to a remaped JTAG DTM.
   * remaps the IR codes, with ir_len = 6
   * @param idcodeIR
   * @param dtmcsIR
   * @param dmiIR
   * @return jtag interface
   */
  def fromRemappedJtag(idcodeIR: Int = 2, dtmcsIR: Int = 3, dmiIR:Int = 34): Jtag = {
    val dtm = new JtagDTM(abits)(6, idcodeIR, dtmcsIR, dmiIR)
    this <> dtm.io.dmi
    dtm.io.jtag
  }

  /**
   *  creates a JTAG trough BSCAN USER4 tunnel.
   *  paired with `riscv use_bscan_tunnel` in openocd, it allows to use the on-chip
   *  bscan prmitive to to create a "virtual" jtag interface.
   */
  def fromTunneledBSCAN() {
    val tunnel = new Bscane2JtagTunnel()
    val jtag = this.fromJtag()
    tunnel.io.jtag <> jtag
  }
}
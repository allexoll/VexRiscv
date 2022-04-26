package vexriscv.plugin.riscvdebug

import spinal.core._
import spinal.lib._
import spinal.lib.blackbox.xilinx.s7.{BSCANE2, BUFG}
import spinal.lib.com.jtag._


/**
 * Creates a DTM similat to the JTAG DTM, but mapped to BSCANE2 USER1/2/3
 * @param memAddressWidth dmi bus address width (`abits`)
 */
class BscaneDTM(memAddressWidth: Int = 7) extends Component{

  val io = new Bundle{
    val dmi = master(DMI(memAddressWidth)) // address width = 32 bits
  }


  val system = new Area {
    val dtmcs = new Area {
      val dmihardreset = Bool()
      val dmireset = Bool()
      val idle = UInt(3 bits)
      val dmistat = UInt(2 bits)
      val abits = UInt(6 bits)
      val version = UInt(4 bits)
      dmihardreset := False
      dmireset := False
      idle := 7
      dmistat := 0
      abits := memAddressWidth
      version := 1 // version 0.13 and 1.0

    }
    // Aggregate dtmcs
    val dtmcs_agr = Bits(32 bits)
    dtmcs_agr(31 downto 18) := 0
    dtmcs_agr(17) := dtmcs.dmihardreset
    dtmcs_agr(16) := dtmcs.dmireset
    dtmcs_agr(15) := False
    dtmcs_agr(14 downto 12) := dtmcs.idle.asBits
    dtmcs_agr(11 downto 10) := dtmcs.dmistat.asBits
    dtmcs_agr(9 downto 4) := dtmcs.abits.asBits
    dtmcs_agr(3 downto 0) := dtmcs.version.asBits
  }

  val dmi_capture = Bits(memAddressWidth + 32 + 2 bits)
  val dmi_update = Bits(memAddressWidth + 32 + 2 bits)

  dmi_capture.clearAll()  // default assignement
  val capture_ready = Bool()
  val update_valid = Bool()

  val idcode_ir = BSCANE2(1) // USER1 remaps to IR = 2
  val dtmcs_ir = BSCANE2(2) // USER2 remaps to IR = 3
  val dmi_ir = BSCANE2(3)   // USER3 remaps to IR = 34

  val jtag = ClockDomain(BUFG.on(idcode_ir.TCK))(new Area{
    /*
     This register is selected (in IR) when the TAP state machine is reset. Its definition is exactly as defined in IEEE Std 1149.1-2013.
     This entire register is read-only.
     ID code should be [version(4bits):partNumber(16):manufID(11):0b1]
      the lsb of idcode should be 1 because the tap wil be bypassed if it is not
     */
    val idcode = new JtagTapInstructionIdcode(B"x10001FFF")
    idcode.ctrl << idcode_ir.toJtagTapInstructionCtrl()

    /*
    The size of this register will remain constant in future versions so that a debugger can always
    determine the version of the DTM.
    */
    val dtmcs = new JtagTapInstructionRead(system.dtmcs_agr, light = false)
    dtmcs.ctrl << dtmcs_ir.toJtagTapInstructionCtrl()
    /*
    This register allows access to the Debug Module Interface (DMI).
    */
    val dmi = new JtagTapInstructionReadWrite(dmi_capture.addTag(crossClockDomain), dmi_update.addTag(crossClockDomain), capture_ready.addTag(crossClockDomain))
    update_valid := dmi.ctrl.enable && dmi.ctrl.update
    dmi.ctrl << dmi_ir.toJtagTapInstructionCtrl()

  })
  // Address is registered so we can capture-dr it next shift.
  // unclear if that is the inted way to do it, issue opened: https://github.com/riscv/riscv-debug-spec/issues/678
  val address = Reg(Bits(memAddressWidth bits))
  address := dmi_update(memAddressWidth + 33 downto 34)
  io.dmi.req.payload.address := dmi_update(memAddressWidth + 33 downto 34)
  io.dmi.req.payload.data := dmi_update(33 downto 2)
  io.dmi.req.payload.op := dmi_update(1 downto 0)
  // updateValid is CC'd and rised to get a single cycle long pulse, in the mainClk domain
  io.dmi.req.valid := BufferCC(update_valid).rise()


  val rsp = io.dmi.rsp

  // capture_ready is rised to get a single clock cycle. it is not CC'd because JTCLK should be much much lower than mainClk
  rsp.ready := capture_ready.rise() && io.dmi.req.op === B"01"
  when(rsp.valid) {
    dmi_capture := address ## rsp.payload.data ## rsp.payload.op
  }
}

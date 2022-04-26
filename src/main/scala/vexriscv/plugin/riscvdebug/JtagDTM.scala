package vexriscv.plugin.riscvdebug

import spinal.core._
import spinal.lib._
import spinal.lib.blackbox.xilinx.s7.BUFG
import spinal.lib.com.jtag._

/*
riscv-debug-stable.pdf chapter 6.1
Debug Transport Modules provide access to the DM over one or more transports (e.g. JTAG or USB)
There may be multiple DTMs in a single hardware platform. Ideally every component that communicates with the outside world includes a DTM, allowing a hardware platform to be debugged through every transport it supports. For instance a USB component could include a DTM. This would trivially allow any hardware platform to be debugged over USB. All that is required is that the USB module already in use also has access to the Debug Module Interface.
Using multiple DTMs at the same time is not supported. It is left to the user to ensure this does not happen.
This specification defines a JTAG DTM in Section 6.1. Additional DTMs may be added in future versions of this specification.
An implementation can be compliant with this specification without implementing any of this section. In that case it must be advertised as conforming to “RISC-V Debug Specification 1.0.0- STABLE, with custom DTM.” If the JTAG DTM described here is implemented, it must be advertised as conforming to the “RISC-V Debug Specification 1.0.0-STABLE, with JTAG DTM.”


This Debug Transport Module is based around a normal JTAG Test Access Port (TAP). The JTAG TAP allows access to arbitrary JTAG registers by first selecting one using the JTAG instruction register (IR), and then accessing it through the JTAG data register (DR).
*/

// memAddressWidth = Minimum of 7 bits. it refers to the address space of the DM interface
class JtagDTM(memAddressWidth: Int = 7)(IRWidth: Int = 5, idcodeIR: Int = 1, dtmcsIR: Int = 0x10, dmiIR:Int = 0x11) extends Component {

  val io = new Bundle{
    val jtag = slave(Jtag())
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

  val jtagCd = ClockDomain(io.jtag.tck)(new Area {
    val tap = new JtagTap(io.jtag, IRWidth)
    /*
     This register is selected (in IR) when the TAP state machine is reset. Its definition is exactly as defined in IEEE Std 1149.1-2013.
     This entire register is read-only.
     ID code should be [version(4bits):partNumber(16):manufID(11):0b1]
      */
    val idcodeArea = tap.idcode(B"x10001FFF")(idcodeIR) // to identify a specific silicon version
    // the lsb of idcode should be 1 because the tap wil be bypassed if it is not
    /*
    The size of this register will remain constant in future versions so that a debugger can always
    determine the version of the DTM.
     */
    val dtmcs = tap.read(system.dtmcs_agr)(dtmcsIR) // For debugging

    /*
    This register allows access to the Debug Module Interface (DMI).
     */
    val dmi = tap.readAndWrite(dmi_capture.addTag(crossClockDomain), dmi_update.addTag(crossClockDomain), capture_ready.addTag(crossClockDomain), update_valid)(dmiIR)
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


//══════════════════════════════════════════════════════════════════════════════
// define JtagDTM object
//
object JtagDTM {
  def main(args: Array[String]) {
    SpinalVerilog(new JtagDTM()())
  }
}

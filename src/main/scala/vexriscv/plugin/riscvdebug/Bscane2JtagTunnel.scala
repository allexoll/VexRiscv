package vexriscv.plugin.riscvdebug

import spinal.core._
import spinal.lib._
import spinal.lib.blackbox.xilinx.s7.{BSCANE2, BUFGCE}
import spinal.lib.com.jtag.Jtag

/**
 * Creates a Jtag Tunnel from a BSCAN2 USER4 primitive.
 * to be used with `riscv use_bscan_tunnel`
 *
 * Essentially this just increases the width of DR with extra bits
 * for the "tunneled" jtag IR
 */
class Bscane2JtagTunnel() extends Component {
  val io = new Bundle{
    val jtag = master(Jtag())
  }
  val bscane2 = BSCANE2(4)

  val bufcge = BUFGCE()
  bufcge.O <> io.jtag.tck
  bufcge.CE <> bscane2.SEL
  bufcge.I <> bscane2.TCK

  io.jtag.tdi := bscane2.TDI

  bscane2.TDO := io.jtag.tdo

  val pos = ClockDomain(bscane2.TCK, reset = !bscane2.SHIFT)(new Area{
    val shiftreg_cnt = Reg(UInt(7 bit)).addTag(crossClockDomain) init 0
    val counter_pos = Reg(UInt(8 bit)).addTag(crossClockDomain) init 0
    val TDI_REG = Reg(Bool()).addTag(crossClockDomain) init False

    counter_pos := counter_pos + 1
    when((counter_pos >= 1) && (counter_pos <= 7)){
      shiftreg_cnt := (bscane2.TDI ## shiftreg_cnt(6 downto 1)).asUInt
    }
    when(counter_pos === 0){
      TDI_REG := !bscane2.TDI
    }
  })
  val neg = ClockDomain(bscane2.TCK, reset = !bscane2.SHIFT).withRevertedClockEdge()(new Area{
    val counter_neg = Reg(UInt(8 bit)).addTag(crossClockDomain) init 0
    counter_neg := counter_neg + 1
  })

  when(neg.counter_neg === 4){
    io.jtag.tms := pos.TDI_REG
  } .elsewhen(neg.counter_neg === 5){
    io.jtag.tms := True
  }.elsewhen((neg.counter_neg === (8 + pos.shiftreg_cnt)) || (neg.counter_neg === 8 + pos.shiftreg_cnt -1 )){
    io.jtag.tms := True
  }.otherwise{
    io.jtag.tms := False
  }
}
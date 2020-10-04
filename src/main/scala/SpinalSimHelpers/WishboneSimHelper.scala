package SpinalSimHelpers

import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.wishbone._

object WishboneHelper {

  def wbWrite(address : BigInt, data : BigInt, wb : Wishbone, clock : ClockDomain) : Unit = {
    wb.ADR #= address
    wb.DAT_MOSI #= data
    wb.WE #= true
    wb.CYC #= true
    wb.STB #= true
    clock.waitSamplingWhere(wb.ACK.toBoolean)
    wb.ADR #= 0
    wb.DAT_MOSI #= 0
    wb.WE #= false
    wb.CYC #= false
    wb.STB #= false
    clock.waitSampling()
  }

  def wbRead(address : BigInt, wb : Wishbone, clock : ClockDomain) : BigInt = {
    wb.ADR #= address
    wb.WE #= false
    wb.CYC #= true
    wb.STB #= true
    clock.waitSamplingWhere(wb.ACK.toBoolean)
    wb.ADR  #= 0
    wb.WE #= false
    wb.CYC #= false
    wb.STB #= false
    wb.DAT_MISO.toBigInt
  } 

}

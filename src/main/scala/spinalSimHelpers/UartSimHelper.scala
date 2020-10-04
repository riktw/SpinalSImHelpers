package spinalSimHelper

import spinal.core._
import spinal.core.sim._

object UartHelper {

  def transmitUartLogic(buffer : Int, baudPeriod : Int, uartPin : Bool) = {
    uartPin #= false
    sleep(baudPeriod)

    (0 to 7).foreach{ bitId =>
    uartPin #= ((buffer >> bitId) & 1) != 0
    sleep(baudPeriod)
    }

    uartPin #= true
    sleep(baudPeriod)
  }

  def transmitUart(data : Int, baudPeriod : Int, uartPin : Bool) = {
      val sendTxThread = fork(transmitUartLogic(data, baudPeriod, uartPin))
      sendTxThread.join()
  }

  def transmitUartAsync(data : Int, baudPeriod : Int, uartPin : Bool) = {
      val sendTxThread = fork(transmitUartLogic(data, baudPeriod, uartPin))
  }

  def receiveUartLogic(expectedByte : Int, baudPeriod : Int, uartPin : Bool) = {
    //Wait until the design put the uartPin to true (wait the reset effect)
    waitUntil(uartPin.toBoolean == true)

    waitUntil(uartPin.toBoolean == false)
    sleep(baudPeriod/2)

    assert(uartPin.toBoolean == false)
    sleep(baudPeriod)

    var buffer = 0
    for(bitId <- 0 to 7) {
      if(uartPin.toBoolean)
        buffer |= 1 << bitId
      sleep(baudPeriod)
    }

    assert(uartPin.toBoolean == true)
    assert(buffer == expectedByte)
  }

  def receiveUart(expectedByte : Int, baudPeriod : Int, uartPin : Bool) = {
      val sendRxThread = fork(receiveUartLogic(expectedByte, baudPeriod, uartPin))
      sendRxThread.join()
  }

  def receiveUartAsync(expectedByte : Int, baudPeriod : Int, uartPin : Bool) = {
      val sendRxThread = fork(receiveUartLogic(expectedByte, baudPeriod, uartPin))
  }

  def calculateBaudPeriod(baudRate : Int, clock : Int) : Int = {
    ((1000000*clock)/baudRate)
  } 

}

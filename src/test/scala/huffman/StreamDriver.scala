// SPDX-License-Identifier: Apache-2.0

package huffman.test

import huffman.Stream
import huffman.ReadyValidLastIO

import chisel3._
import chisel3.util._
import chiseltest._

// implicit class, cannot maintain state
class StreamDriver[T <: Data](x: ReadyValidLastIO[T]) {
  // Source (enqueue) functions
  //
  def initSource(): this.type = {
    x.valid.poke(false.B)
    this
  }

  def setSourceClock(clock: Clock): this.type = {
    ClockResolutionUtils.setClock(StreamDriver.streamSourceKey, x, clock)
    this
  }

  protected def getSourceClock: Clock = {
    ClockResolutionUtils.getClock(
      StreamDriver.streamSourceKey,
      x,
      x.ready.getSourceClock
    ) // TODO: validate against bits/valid sink clocks
  }

  def enqueue(data: T, last: Bool): Unit = timescope {
    // TODO: check for init
    x.bits.poke(data)
    x.valid.poke(true.B)
    x.last.poke(last)
    fork
      .withRegion(Monitor) {
        while (x.ready.peek().litToBoolean == false) {
          getSourceClock.step(1)
        }
      }
      .joinAndStep(getSourceClock)
  }

  def enqueueSeq(data: Seq[T]): Unit = timescope {
    for ((elt,i) <- data.zipWithIndex) {
      enqueue(elt, (i==(data.length-1)).B)
    }
  }

  // Sink (dequeue) functions
  //
  def initSink(): this.type = {
    x.ready.poke(false.B)
    this
  }

  def setSinkClock(clock: Clock): this.type = {
    ClockResolutionUtils.setClock(StreamDriver.streamSinkKey, x, clock)
    this
  }

  protected def getSinkClock: Clock = {
    ClockResolutionUtils.getClock(
      StreamDriver.streamSinkKey,
      x,
      x.valid.getSourceClock
    ) // TODO: validate against bits/valid sink clocks
  }

  // NOTE: this doesn't happen in the Monitor phase, unlike public functions
  def waitForValid(): Unit = {
    while (x.valid.peek().litToBoolean == false) {
      getSinkClock.step(1)
    }
  }

  def expectDequeue(data: T, last: Bool): Unit = timescope {
    // TODO: check for init
    x.ready.poke(true.B)
    fork
      .withRegion(Monitor) {
        waitForValid()
        x.valid.expect(true.B)
        x.last.expect(last)
        x.bits.expect(data)
      }
      .joinAndStep(getSinkClock)
  }

  def expectDequeueSeq(data: Seq[T]): Unit = timescope {
    for ((elt,i) <- data.zipWithIndex) {
      expectDequeue(elt, (i==(data.length-1)).B)
    }
  }

  def expectPeek(data: T): Unit = {
    fork.withRegion(Monitor) {
      x.valid.expect(true.B)
      x.bits.expect(data)
    }
  }

  def expectInvalid(): Unit = {
    fork.withRegion(Monitor) {
      x.valid.expect(false.B)
    }
  }
}

object StreamDriver {
  protected val streamSourceKey = new Object()
  protected val streamSinkKey = new Object()
}

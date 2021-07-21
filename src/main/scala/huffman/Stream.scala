// SPDX-License-Identifier: Apache-2.0

/** Wrappers for ready-valid (Stream) interfaces and associated circuit generators using them.
  */

package huffman

import chisel3._
import chisel3.experimental.{DataMirror, Direction, requireIsChiselType}
import chisel3.internal.naming._  // can't use chisel3_ version because of compile order
import chisel3.internal.Builder  // can't use chisel3_ version because of compile order

/** An I/O Bundle containing 'valid' and 'ready' signals that handshake
  * the transfer of data stored in the 'bits' subfield.
  * The base protocol implied by the directionality is that
  * the producer uses the interface as-is (outputs bits)
  * while the consumer uses the flipped interface (inputs bits).
  * The actual semantics of ready/valid are enforced via the use of concrete subclasses.
  * @param gen the type of data to be wrapped in Ready/Valid
  */
abstract class ReadyValidLastIO[+T <: Data](gen: T) extends Bundle
{
  // // Compatibility hack for rocket-chip
  // private val genType = (DataMirror.internal.isSynthesizable(gen), Builder.currentModule) match {
  //   case (true, Some(module: MultiIOModule))
  //       if !module.compileOptions.declaredTypeMustBeUnbound => chiselTypeOf(gen)
  //   case _ => gen
  // }

  val ready = Input(Bool())
  val valid = Output(Bool())
  val last  = Output(Bool())
  val bits  = Output(gen)
}

object ReadyValidLastIO {

  implicit class AddMethodsToReadyValidLast[T<:Data](target: ReadyValidLastIO[T]) {

    /** Indicates if IO is both ready and valid
     */
    def fire(): Bool = target.ready && target.valid

    /** Push dat onto the output bits of this interface to let the consumer know it has happened.
      * @param dat the values to assign to bits.
      * @return    dat.
      */
    def enq(dat: T): T = {
      target.valid := true.B
      target.bits := dat
      dat
    }

    /** Indicate no enqueue occurs. Valid is set to false, and bits are
      * connected to an uninitialized wire.
      */
    def noenq(): Unit = {
      target.valid := false.B
      target.bits := DontCare
    }

    /** Assert ready on this port and return the associated data bits.
      * This is typically used when valid has been asserted by the producer side.
      * @return The data bits.
      */
    def deq(): T = {
      target.ready := true.B
      target.bits
    }

    /** Indicate no dequeue occurs. Ready is set to false.
      */
    def nodeq(): Unit = {
      target.ready := false.B
    }
  }
}

/** A concrete subclass of ReadyValidLastIO signaling that the user expects a
  * "decoupled" interface: 'valid' indicates that the producer has
  * put valid data in 'bits', and 'ready' indicates that the consumer is ready
  * to accept the data this cycle. No requirements are placed on the signaling
  * of ready or valid.
  * @param gen the type of data to be wrapped in StreamIO
  */
class StreamIO[+T <: Data](gen: T) extends ReadyValidLastIO[T](gen)
{
  override def cloneType: this.type = new StreamIO(gen).asInstanceOf[this.type]
}

/** This factory adds a decoupled handshaking protocol to a data bundle. */
object Stream
{
  /** Wraps some Data with a StreamIO interface. */
  def apply[T <: Data](gen: T): StreamIO[T] = new StreamIO(gen)

  // TODO: use a proper empty data type, this is a quick and dirty solution
  private final class EmptyBundle extends Bundle

  // Both of these methods return StreamIO parameterized by the most generic type: Data
  /** Returns a [[StreamIO]] inteface with no payload */
  def apply(): StreamIO[Data] = apply(new EmptyBundle)
  /** Returns a [[StreamIO]] inteface with no payload */
  def empty: StreamIO[Data] = Stream()

}



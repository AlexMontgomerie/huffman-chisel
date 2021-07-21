package huffman.decoder_test

import huffman.Decoder
import huffman.BufferedDecoder
import huffman.test.StreamDriver

import org.scalatest._

import chisel3._
import chiseltest._
import chisel3.util._

import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.VerilatorBackendAnnotation
import chiseltest.internal.WriteVcdAnnotation

class DecoderTest extends FlatSpec with ChiselScalatestTester with Matchers {

  // function to run tests
  def run_test (seq_in: Seq[UInt], seq_out: Seq[UInt], description: String) {

    // testing annotations
    val annotations = Seq(VerilatorBackendAnnotation,WriteVcdAnnotation)

    behavior of "Decoder"
    it should s"be correct for $description (Decoder)" in {
      // create the DUT
      test(new BufferedDecoder(UInt(8.W), 256, 10, 4,
        "examples/code_table.dat", "examples/len_table.dat")).withAnnotations(annotations) { c =>

        // convert to stream interfaces to StreamDriver
        val in  = new StreamDriver(c.io.in)
        val out = new StreamDriver(c.io.out)

        // module setup
        in.initSource().setSourceClock(c.clock)
        out.initSink().setSinkClock(c.clock)

        // run the sequences
        parallel(
          in.enqueueSeq(seq_in),
          out.expectDequeueSeq(seq_out)
        )
        // c.io.out.ready.poke(true.B)
        // fork { in.enqueueSeq(seq_in) }.join

      }
    }
  }

  // create test sequences
  var description = "a stream of only zeros"
  var seq_in = Seq(0.U, 0.U)
  var seq_out = Seq(0.U, 0.U, 0.U, 0.U)
  run_test(seq_in, seq_out, description)

  description = "a stream of only ones"
  seq_in = Seq(0x04, 0x41, 0x10, 0x4, 0x41, 0x10, 0x4, 0x41, 0x10, 0x4, 0x41, 0x10).map(_.U)
  seq_out = Seq(0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01).map(_.U)
  run_test(seq_in, seq_out, description)

  description = "a stream of alternating zeros and ones"
  seq_in = Seq(0x04, 0x10, 0x40, 0x00, 0x01).map(_.U)
  seq_out = Seq(0x01, 0x00, 0x01, 0x00, 0x01, 0x00, 0x01, 0x00).map(_.U)
  run_test(seq_in, seq_out, description)

}

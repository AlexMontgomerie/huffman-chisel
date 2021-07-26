package huffman.encoder_test

import huffman.Encoder
import huffman.BufferedEncoder
import huffman.test.StreamDriver

import org.scalatest._

import chisel3._
import chiseltest._
import chisel3.util._

import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.VerilatorBackendAnnotation
import chiseltest.internal.WriteVcdAnnotation

class EncoderTest extends FlatSpec with ChiselScalatestTester with Matchers {

  // function to run tests
  def run_test (seq_in: Seq[UInt], seq_out: Seq[UInt], description: String) {

    // testing annotations
    val annotations = Seq(VerilatorBackendAnnotation,WriteVcdAnnotation)

    behavior of "Encoder"
    it should s"be correct for $description (Encoder)" in {
      // create the DUT
      test(new BufferedEncoder(UInt(8.W), "examples/code_table.hex", "examples/len_table.hex")).withAnnotations(annotations) { c =>

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
  var seq_in = Seq(0x00, 0x00, 0x00, 0x00).map(_.U)
  var seq_out = Seq(0x00, 0x00).map(_.U)
  run_test(seq_in, seq_out, description)

  description = "a stream of only ones"
  seq_in = Seq(0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01).map(_.U)
  seq_out = Seq(0x10, 0x41, 0x04, 0x10, 0x41, 0x04).map(_.U)
  run_test(seq_in, seq_out, description)

  description = "a stream of alternating zeros and ones"
  seq_in = Seq(0x01, 0x00, 0x01, 0x00, 0x01, 0x00, 0x01, 0x00).map(_.U)
  seq_out = Seq(0x10, 0x04, 0x01, 0x00, 0x40).map(_.U)
  run_test(seq_in, seq_out, description)

  description = "a stream of random symbols"
  seq_in = Seq(0xc3, 0x87, 0x11, 0xc2, 0xa4, 0xc3, 0xab, 0x0b, 0x0f, 0xc2, 0x84, 0x5d, 0x2f, 0x0a).map(_.U)
  seq_out = Seq(0xbb, 0x87, 0x20, 0xfa, 0xd1, 0xdd, 0xfc, 0x06, 0xc7, 0xfe, 0xb0, 0x8c, 0x27, 0x43).map(_.U)
  run_test(seq_in, seq_out, description)

}

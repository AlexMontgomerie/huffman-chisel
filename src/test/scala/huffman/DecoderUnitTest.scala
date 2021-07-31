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
      test(new BufferedDecoder(UInt(8.W), "examples/code_table.dat", "examples/len_table.dat")).withAnnotations(annotations) { c =>

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
  var seq_in = Seq(0x00, 0x00).map(_.U)
  var seq_out = Seq(0x00, 0x00, 0x00, 0x00).map(_.U)
  run_test(seq_in, seq_out, description)

  // description = "a stream of only ones"
  // seq_in = Seq(0x10, 0x41, 0x04, 0x10, 0x41, 0x04).map(_.U)
  // seq_out = Seq(0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01).map(_.U)
  // run_test(seq_in, seq_out, description)

  // description = "a stream of alternating zeros and ones"
  // seq_in = Seq(0x10, 0x04, 0x01, 0x00, 0x40, 0x10, 0x04, 0x01, 0x00, 0x40).map(_.U)
  // seq_out = Seq(0x01, 0x00, 0x01, 0x00, 0x01, 0x00, 0x01, 0x00, 0x01, 0x00, 0x01, 0x00, 0x01, 0x00, 0x01, 0x00).map(_.U)
  // run_test(seq_in, seq_out, description)

  description = "a stream of random symbols"
  seq_in = Seq( 0xbb, 0x8a, 0xbb, 0x85, 0x14, 0xbb, 0x9a, 0x6c, 0xbb, 0xab, 0x76, 0xfa, 0xdc, 0x5d, 0xfc, 0xae,
                0xe3, 0x7e, 0xb5, 0x17, 0x72, 0xff, 0x58, 0xaf, 0xac, 0x55, 0xdc, 0xbd, 0xdc, 0xfd, 0xdc, 0x75,
                0xdc, 0x25, 0xdd, 0x11, 0x8d, 0xdd, 0x13, 0x97, 0xd6, 0x9b, 0xeb, 0x3d, 0x77, 0x69, 0xf5, 0xaa,
                0x2e, 0x40, 0xfa, 0xfc, 0xee, 0xe4, 0xee, 0xe5, 0xee, 0xe2, 0x3b, 0xff, 0x5a, 0x7f, 0x37, 0xd6, 0x94, 0x68).map(_.U)
  seq_out = Seq(0xc3, 0x8a, 0xc3, 0x85, 0x02, 0xc3, 0x9a, 0x6a, 0xc3, 0xaf, 0x75, 0xc2, 0xbe, 0xc3, 0xb7, 0xc3,
                0x8d, 0xc2, 0xaa, 0xc3, 0x97, 0xc2, 0x8a, 0xc2, 0x8a, 0xc3, 0x97, 0xc3, 0xa0, 0xc3, 0x8e, 0xc3,
                0x84, 0xc3, 0xa3, 0x24, 0xc3, 0xa3, 0x70, 0xc2, 0xa8, 0xc2, 0x9f, 0xc3, 0xba, 0xc2, 0xad, 0x20,
                0x36, 0xc2, 0xb8, 0xc3, 0x93, 0xc3, 0x97, 0xc3, 0x88, 0x1a, 0xc2, 0xa9, 0x4d, 0xc2, 0xa7, 0x0a).map(_.U)
  run_test(seq_in, seq_out, description)

  // description = "a stream of random symbols (even)"
  // seq_in = Seq( 0xbb, 0xab, 0x76, 0xfa, 0xdc, 0x5d, 0xfc, 0xae, 0xe3, 0x7e, 0xb5, 0x17, 0x72, 0xff, 0x58, 0xab,
  //               0xb8, 0xab, 0xb8, 0x51, 0x4b, 0xb9, 0xa6, 0xcb, 0xba, 0xb7, 0x6f, 0xad, 0xc5, 0xdf, 0xca, 0xee,
  //               0x37, 0xeb, 0x51, 0x77, 0x2f, 0xf5, 0x8a, 0x1a).map(_.U)
  // seq_out = Seq(0xc3, 0xaf, 0x75, 0xc2, 0xbe, 0xc3, 0xb7, 0xc3, 0x8d, 0xc2, 0xaa, 0xc3, 0x97, 0xc2, 0x8a, 0xc3,
  //               0x8a, 0xc3, 0x85, 0x02, 0xc3, 0x9a, 0x6a, 0xc3, 0xaf, 0x75, 0xc2, 0xbe, 0xc3, 0xb7, 0xc3, 0x8d,
  //               0xc2, 0xaa, 0xc3, 0x97, 0xc2, 0x8a, 0x0a).map(_.U)
  // run_test(seq_in, seq_out, description)

}

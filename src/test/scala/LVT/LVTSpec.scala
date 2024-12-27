// See README.md for license details.

package LVT

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.Random

// TODO: Parameterize this test
class LVTSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  val nRead=4;
  val mWrite=4;
  val depth=128;
  val width=32;

  behavior of "LVTMultiPortRams"
  it should "correctly handle random read/write transactions on all ports" in {
    // Adjust n, m, depth, and width as needed

    /*
    * Spec:
    * Reads return 1 cycle later
    * Reads and writes to the same address on the same cycle returns the currently writing value
    * More than 1 write to the same address per cycle results in undefined behavior, even if the write value is identical
    */

    test(new nReadmWriteLVT(n = nRead, m = mWrite, depth = depth, width = width))
      .withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      val goldenMem = Array.fill[BigInt](depth)(BigInt(0))

      // Reset the DUT
      dut.clock.step(5)

      // Number of random test iterations
      val numIters = 1000

      for (i <- 0 until numIters) {
        println(s"test cycle ${i}")

        // Generate random data
        val readAddresses  = Random.shuffle(0 to 32).take(4).map(_.U(32.W))
        val writeAddresses = Random.shuffle(0 to 32).take(4).map(_.U(32.W))
        val writeData      = Seq.fill(4)(BigInt(32, Random).U(32.W))
        val writeEnable    = Seq.fill(4)(Random.nextBoolean())


        // Drive DUT inputs
        dut.io.rdAddr.zip(readAddresses).foreach  { case (rd, addr) => rd.poke(addr) }
        dut.io.wrAddr.zip(writeAddresses).foreach { case (wr, addr) => wr.poke(addr) }
        dut.io.wrData.zip(writeData).foreach      { case (wd, data) => wd.poke(data) }
        dut.io.wrEn.zip(writeEnable).foreach     { case (we, en)   => we.poke(en.B) }

        // Step the clock so writes happen
        dut.clock.step(1)

        // Update the golden memory for writes
        for (writePort <- 0 until writeAddresses.size) {
          if (writeEnable(writePort)) {
            val addr = writeAddresses(writePort).litValue.toInt // Convert UInt to Int
            goldenMem(addr) = writeData(writePort).litValue     // Update golden memory
          }
        }

        // Check reads against the golden memory
        for (readPort <- 0 until readAddresses.size) {
          val addr = readAddresses(readPort).litValue.toInt      // Convert UInt to Int
          val expectedData = goldenMem(addr)                     // Get data from golden memory
          val dutReadData  = dut.io.rdData(readPort).peek().litValue // Peek DUT read data
          withClue(s"Read Port $readPort at address 0x${addr.toHexString}: ") {
            dutReadData shouldBe expectedData                    // Compare DUT output with golden memory
          }
        }
      }





    }
  }
}



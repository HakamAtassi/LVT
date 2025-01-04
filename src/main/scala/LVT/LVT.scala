package LVT

import chisel3._
import chisel3.util._


// The LVT "Banks"
// Just a series of n read 1 write memories
class nRead1Write(n: Int, depth: Int, width: Int) extends Module {
  val addrW = log2Ceil(depth) // Address width derived from depth
  val io = IO(new Bundle {
    val wrAddr = Input(UInt(addrW.W))
    val wrData = Input(UInt(width.W))
    val wrEn = Input(Bool())

    val rdAddr = Input(Vec(n, UInt(addrW.W)))
    val rdData = Output(Vec(n, UInt(width.W)))
  })

  // a series of dual ported memories
  // 1 memory per read port
  // all writes tied
  // writes forwarded

  // Create n memories, one for each read port
  val memories = Seq.fill(n)(SyncReadMem(depth, UInt(width.W)))

  for (mem <- memories) {
    when(io.wrEn) {
      mem.write(io.wrAddr, io.wrData)
    }
  }

  // output+forward data
  io.rdData := VecInit(memories.zip(io.rdAddr).map { case (mem, addr) => Mux(RegNext(io.wrAddr===addr && io.wrEn), RegNext(io.wrData), mem.read(addr))})
}

class nReadmWriteLVT(n: Int, m: Int, depth: Int, width: Int) extends Module {
  val addrW: Int = log2Ceil(depth)

  val io = IO(new Bundle {
    val wrAddr = Input(Vec(m, UInt(addrW.W))) // Write addresses for m write ports
    val wrData = Input(Vec(m, UInt(width.W))) // Write data for m write ports
    val wrEn = Input(Vec(m, Bool()))         // Write enables for m write ports

    val rdAddr = Input(Vec(n, UInt(addrW.W))) // Read addresses for n read ports
    val rdData = Output(Vec(n, UInt(width.W))) // Read data for n read ports
  })

  ///////////////////
  // LVT TABLE R/W //
  ///////////////////

  // LVT table stores the bank index of the most recent write for that address

  val LVTTable = RegInit(VecInit(Seq.fill(depth)(0.U(log2Ceil(m).W))))  // LVT table itself
  val LVTTableOut = Reg(Vec(n, UInt(log2Ceil(m).W)))                    // read LVT for writes

  // update LVT mem on writes
  for (i <- 0 until m) {
    when(io.wrEn(i)) {
      LVTTable(io.wrAddr(i)) := i.U 
    }
  }

  // read LVT and forward as needed (write first)
  for (i <- 0 until n) {
    LVTTableOut(i) := LVTTable(io.rdAddr(i)) // Default: read from the table
    for (j <- 0 until m) {
      when(io.wrAddr(j) === io.rdAddr(i) && io.wrEn(j)) {
        LVTTableOut(i) := j.U 
      }
    }
  }


  
  // each LVT bank is just an nRead1Write memory
  val LVTBanks = Seq.fill(m)(Module(new nRead1Write(n, depth, width)))

  // each LVT memory performs that port's write, but does ALL reads
  for (i <- 0 until m) {
    val bank = LVTBanks(i)
    bank.io.wrAddr := io.wrAddr(i)
    bank.io.wrData := io.wrData(i)
    bank.io.wrEn := io.wrEn(i)
    bank.io.rdAddr := io.rdAddr
  }

  // Output Mux: Select data from the appropriate bank for each read port


  for (i <- 0 until n) {  // for each output read port
    val selectedBankIndex = LVTTableOut(i)
    io.rdData(i) := 0.U
    for(j <- 0 until m){  // search banks
      val bank = LVTBanks(j)
      when(j.U === selectedBankIndex){
        io.rdData(i) := bank.io.rdData(i) 
      }
    }
  }


}

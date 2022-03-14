package gps
import chisel3._
import chisel3.util._
import chisel3.experimental

import spire._
import spire.implicits._
import spire.math.extras.{FixedPoint, FixedScale}
import spire.std.double

class MatMulIO(p: GPSParams, aRows: Int, bRows: Int, cRows: Int, aCols: Int, bCols: Int, cCols: Int) extends Bundle {
  val in = Flipped(Decoupled(new Bundle {
    val a = Vec(aRows, Vec(aCols, experimental.FixedPoint(p.width.W, p.bp.BP)))
    val b = Vec(bRows, Vec(bCols, experimental.FixedPoint(p.width.W, p.bp.BP)))
  }))
  val out = Valid(Vec(cRows, Vec(cCols, experimental.FixedPoint(p.width.W, p.bp.BP))))
}



class MatrixMul(p: GPSParams, cyclesPerTransfer: Int, parallelism: Int, aRows: Int, bRows: Int, cRows: Int, aCols: Int, bCols: Int, cCols: Int, aElementsPerTransfer: Int, bElementsPerTransfer: Int, cElementsPerTransfer: Int) extends Module {
  require(cyclesPerTransfer == 1)
  require(cCols >= parallelism)
  require(cCols % parallelism == 0)
  val io = IO(new MatMulIO(p, aRows, bRows, cRows, aCols, bCols, cCols))
  val a = Reg(Vec(aRows, Vec(aCols, experimental.FixedPoint((p.width).W, p.bp.BP))))
  val b = Reg(Vec(bRows, Vec(bCols, experimental.FixedPoint((p.width).W, p.bp.BP))))
  val c = Reg(Vec(cRows, Vec(cCols, experimental.FixedPoint((p.width).W, p.bp.BP))))
  val busy = RegInit(false.B)
  io.in.ready := !busy
  io.out.valid := !busy
  when (io.in.fire) {
    busy := true.B
    a := io.in.bits.a
    b := io.in.bits.b
    for (i <- 0 until a.size) {
      for (j <- 0 until b(0).size) {
        c(i)(j) := 0.U.asFixedPoint(p.bp.BP)
      }
    }
//    c foreach { _ foreach { _ := 0.S }}
//    for (rowIndex <- 0 until p.cRows) {
//      for (colIndex <- 0 until p.cCols) {
//        c(rowIndex)(colIndex) := 0.S
//      }
//    }
  }
  io.out.bits := c
  val countRst = !busy && io.in.fire
  val (kIndex, kWrap) = Counter(0 until aCols, true.B, countRst)
  val kMax = kIndex === (aCols-1).U
  val (colIndex, colWrap) = Counter(0 until cCols by parallelism, kMax, countRst)
  val colMax = colIndex === (cCols-parallelism).U
  val (rowIndex, rowWrap) = Counter(0 until cRows, kMax && colMax, countRst)
  val rowMax = rowIndex === (cRows-1).U
  when (busy) {
    for (core <- 0 until parallelism) {
      c(rowIndex)(colIndex + core.U) := c(rowIndex)(colIndex + core.U) + a(rowIndex)(kIndex) * b(kIndex)(colIndex + core.U)
    }
  }
  when (busy && kMax && rowMax && colMax) {
    busy := false.B
  }
}
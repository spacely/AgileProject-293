package gps
import chisel3._
import chisel3.util._
import chisel3.experimental

import spire._
import spire.implicits._
import spire.math.extras.{FixedPoint, FixedScale}
import spire.std.double

class FixedToChiselModelIO(p: GPSParams) extends Bundle {
    val out = Output(experimental.FixedPoint(p.width.W, p.bp.BP))
}

class FixedToChiselModel(f: FixedPoint, p: GPSParams) extends Module {

    val io = IO(new FixedToChiselModelIO(p))
    implicit val scale = p.scale
    val value = p.fixedToChisel(f)
    io.out := value
   
}
package gps

import chisel3._
import chisel3.util._
import chisel3.experimental

import spire._
import spire.implicits._
import spire.math.extras.{FixedPoint, FixedScale}

class GPSCalculatorIO(p: GPSParams) extends Bundle{

val input_tranpose = IO(new MatrixtransposeIO(p))
val input_inverse = IO(new MatrixInverseIO(p))

val out_gps_cal = Output(experimental.FixedPoint(p.width.W, p.bp.BP))


}

class GPSCalulator(p: GPSParams) extends Module {
        val io = IO(new GPSCalculatorIO(p))
        io.out_gps_cal = io.input_tranpose 

}


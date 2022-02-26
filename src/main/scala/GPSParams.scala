package gps

import chisel3._
import chisel3.util._
import chisel3.experimental

import spire._
import spire.implicits._
import spire.math.extras.{FixedPoint, FixedScale}

// FixedPoint refers to Spire fixed point (scala type)
// experimental.FixedPoint refers to chisel type
case class GPSParams(sat_num: Int = 4, width: Int = 8, bp: Int = 4, mat_override: Boolean = false, rows_override: Int = 0, cols_override: Int = 0) {
  
  if(mat_override){
    require(rows_override > 0)
    require(cols_override > 0)
  }
  val rows: Int = if(mat_override) rows_override else sat_num
  val cols: Int = if(mat_override) cols_override else 4
  require(bp >= 1)

  val spire_bp: Int = 1 << bp

  implicit val scale = FixedScale(spire_bp)

  def fixedToChisel(a: FixedPoint, width: Int = width, bp: Int = bp)(implicit scale: FixedScale): experimental.FixedPoint = {
    val chiselFixed = experimental.FixedPoint.fromDouble(a.toDouble, width.W, bp.BP)
    chiselFixed
  }
}

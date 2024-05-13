package domain

import zio.json._
sealed trait BoardCmd

object BoardCmd {
  implicit val encoder: JsonEncoder[BoardCmd] = DeriveJsonEncoder.gen[BoardCmd]
  implicit val decored: JsonDecoder[BoardCmd] = DeriveJsonDecoder.gen[BoardCmd]
}

case class BishopAdded(at: Int)            extends BoardCmd
case class BishopTaken(at: Int)            extends BoardCmd
case class BishopMoved(from: Int, to: Int) extends BoardCmd

case class RookAdded(at: Int)            extends BoardCmd
case class RookTaken(at: Int)            extends BoardCmd
case class RookMoved(from: Int, to: Int) extends BoardCmd

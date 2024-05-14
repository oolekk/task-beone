package domain

import zio.json._
sealed trait BoardCmd {
  def invert: BoardCmd
}

object BoardCmd {
  implicit val encoder: JsonEncoder[BoardCmd] = DeriveJsonEncoder.gen[BoardCmd]
  implicit val decoder: JsonDecoder[BoardCmd] = DeriveJsonDecoder.gen[BoardCmd]
}

case class BishopAdded(at: Int) extends BoardCmd {
  override def invert: BoardCmd = BishopTaken(at)
}
case class BishopTaken(at: Int) extends BoardCmd {

  override def invert: BoardCmd = BishopAdded(at)
}
case class BishopMoved(from: Int, to: Int) extends BoardCmd {
  override def invert: BoardCmd = BishopMoved(to, from)
}

case class RookAdded(at: Int) extends BoardCmd {
  override def invert: BoardCmd = RookTaken(at)
}
case class RookTaken(at: Int) extends BoardCmd {
  override def invert: BoardCmd = RookAdded(at)
}
case class RookMoved(from: Int, to: Int) extends BoardCmd {
  override def invert: BoardCmd = RookMoved(to, from)
}

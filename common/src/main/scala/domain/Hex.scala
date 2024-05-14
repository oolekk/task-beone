package domain

import zio.json._

case class Hex(hex: String) extends AnyVal

object Hex {
  implicit val encoder: JsonEncoder[Hex] = DeriveJsonEncoder.gen[Hex]
  implicit val decoder: JsonDecoder[Hex] = DeriveJsonDecoder.gen[Hex]
}

case class HexList(hxs: List[String] = Nil)

object HexList {
  implicit val encoder: JsonEncoder[HexList] = DeriveJsonEncoder.gen[HexList]
  implicit val decoder: JsonDecoder[HexList] = DeriveJsonDecoder.gen[HexList]
}

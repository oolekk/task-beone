package domain

import zio.json._

case class HexList(hxs: List[String] = Nil)

object HexList {
  implicit val encoder: JsonEncoder[HexList] = DeriveJsonEncoder.gen[HexList]
  implicit val decoder: JsonDecoder[HexList] = DeriveJsonDecoder.gen[HexList]
}


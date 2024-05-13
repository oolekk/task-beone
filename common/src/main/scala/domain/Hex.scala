package domain

import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}

case class Hex private (hex: String) extends AnyVal

object Hex {
  implicit val listEncoder: JsonEncoder[List[Hex]] = DeriveJsonEncoder.gen[List[Hex]]
  implicit val listDecoder: JsonDecoder[List[Hex]] = DeriveJsonDecoder.gen[List[Hex]]
  implicit val encoder: JsonEncoder[Hex]           = DeriveJsonEncoder.gen[Hex]
  implicit val decoder: JsonDecoder[Hex]           = DeriveJsonDecoder.gen[Hex]
}

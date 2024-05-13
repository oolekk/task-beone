package domain

import zio.json.{DeriveJsonDecoder, DeriveJsonEncoder, JsonDecoder, JsonEncoder}

case class Hex private (hex: String) extends AnyVal

object Hex {
  implicit val encoder: JsonEncoder[Hex] = DeriveJsonEncoder.gen[Hex]
  implicit val decored: JsonDecoder[Hex] = DeriveJsonDecoder.gen[Hex]
}

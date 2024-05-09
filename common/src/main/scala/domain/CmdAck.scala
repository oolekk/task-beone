package domain

import zio.json._

case class CmdAck(gameId: String, roundId: String, cmd: Either[String, BoardCmd])

object CmdAck {
  implicit val encoder: JsonEncoder[CmdAck] = DeriveJsonEncoder.gen[CmdAck]
  implicit val decored: JsonDecoder[CmdAck] = DeriveJsonDecoder.gen[CmdAck]
}

package domain

import zio.json._

/*
 * Combines command to be applied with expected resulting game state.
 * This will be used to send board update requests from the client, where
 * client specifies the board action and the expected result. Service can
 * subsequently check that requested action is consistent with stored game
 * and produces the expected result. Only then commit it to db atomically.
 */

case class SnapCmd(cmd: BoardCmd, snap: GameSnap)
object SnapCmd {
  implicit val encoder: JsonEncoder[SnapCmd] = DeriveJsonEncoder.gen[SnapCmd]
  implicit val decoder: JsonDecoder[SnapCmd] = DeriveJsonDecoder.gen[SnapCmd]
}

case class SnapCmds(cmds: List[SnapCmd] = Nil)
object SnapCmds {
  implicit val encoder: JsonEncoder[SnapCmds] = DeriveJsonEncoder.gen[SnapCmds]
  implicit val decoder: JsonDecoder[SnapCmds] = DeriveJsonDecoder.gen[SnapCmds]
}

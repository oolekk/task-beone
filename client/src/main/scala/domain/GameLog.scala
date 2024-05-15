package domain

import zio.json._

case class GameLog(on: List[LogEntry], off: List[LogEntry])

object GameLog {

  val empty: GameLog = GameLog(Nil, Nil)

  implicit val enc: JsonEncoder[GameLog] = DeriveJsonEncoder.gen[GameLog]
  implicit val dec: JsonDecoder[GameLog] = DeriveJsonDecoder.gen[GameLog]

  implicit class GameLogOps(gameLog: GameLog) {

    def describeOne(id: Int, round: Int): Option[String] =
      findOnBoard(id)
        .map(_.describe(round, true))
        .orElse(findOffBoard(id).map(_.describe(round, false)))

    def describeAll(round: Int): List[String] = {
      val offInfo = describeAllOffBoard(round)
      if (offInfo.isEmpty) describeAllOnBoard(round)
      else describeAllOnBoard(round) ::: s"NO LONGER ON THE BOARD:" :: describeAllOffBoard(round)
    }

    private def describeAllOnBoard(round: Int): List[String] =
      gameLog.on.map(_.describe(round, true))

    private def describeAllOffBoard(round: Int): List[String] =
      gameLog.off.map(_.describe(round, false))

    private def findOffBoard(id: Int): Option[LogEntry] = gameLog.off.find(_.id == id)
    private def findOnBoard(id: Int): Option[LogEntry]  = gameLog.on.find(_.id == id)

    def find(id: Int): Option[LogEntry] = findOnBoard(id) orElse findOffBoard(id)
  }

}

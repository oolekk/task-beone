package domain

case class GameLog(on: List[LogEntry], off: List[LogEntry])

object GameLog {

  val empty: GameLog = GameLog(Nil, Nil)

  implicit class GameLogOps(gameLog: GameLog) {

    def describeOne(id: Int, round: Int): Option[String] =
      findOnBoard(id)
        .map(_.describe(round, onBoard = true))
        .orElse(findOffBoard(id).map(_.describe(round, onBoard = false)))

    def describeAll(round: Int): List[String] = {
      val offInfo = describeAllOffBoard(round)
      if (offInfo.isEmpty) describeAllOnBoard(round)
      else describeAllOnBoard(round) ::: s"NO LONGER ON THE BOARD:" :: describeAllOffBoard(round)
    }

    private def describeAllOnBoard(round: Int): List[String] =
      gameLog.on.map(_.describe(round, onBoard = true))

    private def describeAllOffBoard(round: Int): List[String] =
      gameLog.off.map(_.describe(round, onBoard = false))

    private def findOffBoard(id: Int): Option[LogEntry] = gameLog.off.find(_.id == id)
    private def findOnBoard(id: Int): Option[LogEntry]  = gameLog.on.find(_.id == id)

    def find(id: Int): Option[LogEntry] = findOnBoard(id) orElse findOffBoard(id)
  }

}

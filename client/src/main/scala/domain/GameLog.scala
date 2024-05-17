package domain

import domain.GameSnap.rawIndex
import domain.LogEntry.{Bishop, Rook, RoundXY}

case class GameLog(on: List[LogEntry], off: List[LogEntry], lastId: Int = 0)

object GameLog {

  private val NO_SUCH_ID_ON_BOARD_MSG = "No such ID for any piece on the board!"

  val empty: GameLog = GameLog(Nil, Nil)

  def fresh(snap: GameSnap): GameLog = {
    import util.LongBitOps._
    val all = snap.allPieces.find1s
    GameLog(
      on = all.zipWithIndex.map { case (at, idx) =>
        val rxy = RoundXY(0, at)
        if (snap.rooks.getBool(at)) Rook(idx + 1, rxy, rxy, List(at))
        else Bishop(idx + 1, rxy, rxy, List(at))
      },
      off = Nil,
      lastId = all.size
    )
  }

  implicit class GameLogOps(gameLog: GameLog) {

    def applyAddRook(round: Int, xy: (Int, Int)): GameLog = {
      val rxy  = RoundXY(round, xy)
      val rook = Rook(gameLog.lastId + 1, rxy, rxy, List(xy))
      applyAddPiece(rook)
    }

    def applyAddBishop(round: Int, xy: (Int, Int)): GameLog = {
      val rxy    = RoundXY(round, xy)
      val bishop = Bishop(gameLog.lastId + 1, rxy, rxy, List(xy))
      applyAddPiece(bishop)
    }

    private def applyAddPiece(entry: LogEntry): GameLog =
      gameLog.copy(on = entry :: gameLog.on, lastId = entry.id)

    def applyMovePiece(round: Int, xyOld: (Int, Int), xyNew: (Int, Int)): GameLog = {
      val logIdx            = gameLog.on.indexWhere(_.lastAt.at == rawIndex(xyOld))
      val (pre, log :: aft) = gameLog.on.splitAt(logIdx)
      gameLog.copy(on = pre ::: log.moved(round, xyNew) :: aft)
    }

    def applyTakePiece(round: Int, xy: (Int, Int)): GameLog = {
      val logIdx            = gameLog.on.indexWhere(_.lastAt.at == rawIndex(xy))
      val (pre, log :: aft) = gameLog.on.splitAt(logIdx)
      gameLog.copy(on = pre ::: aft, off = log.taken(round) :: gameLog.off)
    }

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

    def findAtById(id: Int): Either[String, Int] =
      gameLog.on.find(_.id == id).map(_.lastAt.at).toRight(NO_SUCH_ID_ON_BOARD_MSG)

  }

}

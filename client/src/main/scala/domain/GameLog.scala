package domain

import util.BoardCmdSolver
import zio.json._

import scala.collection.mutable.ListBuffer

case class GameLog(gameId: String, on: List[LogEntry], off: List[LogEntry])

object GameLog {

  implicit val enc: JsonEncoder[GameLog] = DeriveJsonEncoder.gen[GameLog]
  implicit val dec: JsonDecoder[GameLog] = DeriveJsonDecoder.gen[GameLog]

  implicit class GameLogOps(gameLog: GameLog) {

    private val ON_BOARD_SUFFIX  = " Still on the board."
    private val OFF_BOARD_SUFFIX = " Taken off the board."

    def describe(id: Int): Option[String] =
      findOnBoard(id)
        .map(_.describe(ON_BOARD_SUFFIX))
        .orElse(findOffBoard(id).map(_.describe(OFF_BOARD_SUFFIX)))

    def describeAll: List[String] = {
      val offInfo = describeAllOffBoard
      if (offInfo.isEmpty) describeAllOnBoard
      else describeAllOnBoard ::: s"NO LONGER ON THE BOARD:" :: describeAllOffBoard
    }

    private def describeAllOnBoard: List[String] =
      gameLog.on.map(_.describe(ON_BOARD_SUFFIX))

    private def describeAllOffBoard: List[String] =
      gameLog.off.map(_.describe(OFF_BOARD_SUFFIX))

    private def findOffBoard(id: Int): Option[LogEntry] = gameLog.off.find(_.id == id)
    private def findOnBoard(id: Int): Option[LogEntry]  = gameLog.on.find(_.id == id)

    def find(id: Int): Option[LogEntry] = findOnBoard(id) orElse findOffBoard(id)
  }

  def replay(gameId: String, snaps: Seq[GameSnap]): Either[String, GameLog] = {

    val rooksOnBoard   = ListBuffer[Rook]()
    val bishopsOnBoard = ListBuffer[Bishop]()
    val offBoard       = ListBuffer[LogEntry]()
    var round: Int     = 0
    var nextId: Int    = 1

    val changes: Iterator[Either[String, BoardCmd]] = if (snaps.nonEmpty) {
      (GameSnap.empty :: snaps.toList.reverse)
        .sliding(2, 1)
        .collect { case List(prev, next) => BoardCmdSolver.solve(prev, next) }
    } else Iterator.empty

    // early-finish drop-while trick, explained in LongBitOps
    val dropAsWeGo: Iterator[Either[String, BoardCmd]] = changes.dropWhile {
      case Right(RookAdded(at)) =>
        val rat = RoundXY(round, at)
        rooksOnBoard.prepend(Rook(nextId, rat, rat, List(at))); round += 1; nextId += 1; true
      case Right(BishopAdded(at)) =>
        val rat = RoundXY(round, at)
        bishopsOnBoard.prepend(Bishop(nextId, rat, rat, List(at))); round += 1; nextId += 1; true
      case Right(RookTaken(at)) =>
        findIndexByLastAt(at, rooksOnBoard).exists { idx =>
          offBoard.prepend(rooksOnBoard.remove(idx).copy(lastAt = RoundXY(round, at)))
          round += 1; true
        }
      case Right(BishopTaken(at)) =>
        findIndexByLastAt(at, bishopsOnBoard).exists { idx =>
          offBoard.prepend(bishopsOnBoard.remove(idx).copy(lastAt = RoundXY(round, at)))
          round += 1; true
        }
      case Right(RookMoved(from, to)) =>
        findByLastAt(from, rooksOnBoard).exists { case (idx, entry) =>
          rooksOnBoard.update(idx, entry.copy(lastAt = RoundXY(round, to), moves = to :: entry.moves))
          round += 1
          true
        }
      case Right(BishopMoved(from, to)) =>
        findByLastAt(from, bishopsOnBoard).exists { case (idx, entry) =>
          bishopsOnBoard.update(idx, entry.copy(lastAt = RoundXY(round, to), moves = to :: entry.moves))
          round += 1
          true
        }
      case _ => false
    }

    Either.cond(
      dropAsWeGo.isEmpty,
      GameLog(
        gameId = gameId,
        on = (rooksOnBoard ++ bishopsOnBoard).toList.sortBy(_.id),
        off = offBoard.toList.sortBy(_.id)
      ),
      s"GameLog replay failed at offset: $round"
    )
  }
  private def findByLastAt[E <: LogEntry](at: Int, buffer: ListBuffer[E]): Option[(Int, E)] =
    findIndexByLastAt(at, buffer).map(idx => (idx, buffer(idx)))

  private def findIndexByLastAt[E <: LogEntry](at: Int, buffer: ListBuffer[E]): Option[Int] = {
    val idx = buffer.indexWhere(_.lastAt.at == at)
    Option.when(idx >= 0)(idx)
  }

}

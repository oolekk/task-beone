package domain

import domain.GameSnap.asXY
import util.BoardCmdSolver
import zio.json._

import scala.collection.mutable.ListBuffer

case class GameLog(gameId: String, on: List[Entry], off: List[Entry])

case class Bishop(id: Int, firstAt: RoundXY, lastAt: RoundXY, moves: List[Int] = Nil) extends Entry {
  def describe(suffix: String): String = describe(s"This bishop with id $id ", suffix)
}
case class Rook(id: Int, firstAt: RoundXY, lastAt: RoundXY, moves: List[Int] = Nil) extends Entry {
  def describe(suffix: String): String = describe(s"This rook with id $id ", suffix)
}
case class RoundXY(round: Int, at: Int) {
  def xy: (Int, Int) = asXY(at)
}

sealed trait Entry {
  val id: Int
  val moves: List[Int] // from newest-to-oldest is left-to-right
  val firstAt, lastAt: RoundXY
  def describe(prefix: String): String
  def describe(prefix: String, suffix: String): String =
    s"${prefix}spent ${lastAt.round - firstAt.round} on the game board." +
      s"It was first put on the board in round ${firstAt.round} on field ${asXY(firstAt.at)}." +
      s"It moved ${moves.size} times and travelled total distance of $distance" +
      s"It was last seen in round ${lastAt.round} on field ${asXY(lastAt.at)}.$suffix"

  def distance: Int = (moves.sliding(2, 2) map { case List(i, j) =>
    if ((j - i) % 9 == 0) (j - i) / 9 //      diagonal \\ distance
    else if (i % 8 == j % 8) (j - i) / 8   // vertical distance
    else if ((j - i) % 7 == 0) (j - i) / 7 // diagonal // distance
    else j - i                             // horizontal distance
  }).sum
}

object Entry {
  implicit val entryEnc: JsonEncoder[Entry]     = DeriveJsonEncoder.gen[Entry]
  implicit val entryDec: JsonDecoder[Entry]     = DeriveJsonDecoder.gen[Entry]
  implicit val gameLogEnc: JsonEncoder[GameLog] = DeriveJsonEncoder.gen[GameLog]
  implicit val gameLogDec: JsonDecoder[GameLog] = DeriveJsonDecoder.gen[GameLog]
  implicit val bishopEnc: JsonEncoder[Bishop]   = DeriveJsonEncoder.gen[Bishop]
  implicit val bishopDec: JsonDecoder[Bishop]   = DeriveJsonDecoder.gen[Bishop]
  implicit val rookEnc: JsonEncoder[Rook]       = DeriveJsonEncoder.gen[Rook]
  implicit val rookDec: JsonDecoder[Rook]       = DeriveJsonDecoder.gen[Rook]
  implicit val roundXYenc: JsonEncoder[RoundXY] = DeriveJsonEncoder.gen[RoundXY]
  implicit val roundXYdec: JsonDecoder[RoundXY] = DeriveJsonDecoder.gen[RoundXY]
}

object GameLog {

  implicit val enc: JsonEncoder[GameLog] = DeriveJsonEncoder.gen[GameLog]
  implicit val dec: JsonDecoder[GameLog] = DeriveJsonDecoder.gen[GameLog]

  implicit class GameLogOps(gameLog: GameLog) {

    private val ON_BOARD_SUFFIX  = " He is still on the board."
    private val OFF_BOARD_SUFFIX = " He was taken off the board."

    def describeAllOnBoard: List[String] =
      gameLog.on.map(_.describe(ON_BOARD_SUFFIX))

    def describeAllOffBoard: List[String] =
      gameLog.off.map(_.describe(ON_BOARD_SUFFIX))

    def describe(id: Int): Option[String] =
      findOnBoard(id)
        .map(_.describe(ON_BOARD_SUFFIX))
        .orElse(findOffBoard(id).map(_.describe(OFF_BOARD_SUFFIX)))

    def findOffBoard(id: Int): Option[Entry] = gameLog.off.find(_.id == id)
    def findOnBoard(id: Int): Option[Entry]  = gameLog.on.find(_.id == id)

    def find(id: Int): Option[Entry] = findOnBoard(id) orElse (findOffBoard(id))
  }

  def replayFromSnaps(gameId: String, snaps: Seq[GameSnap]): Either[String, GameLog] = {

    val rooksOnBoard   = ListBuffer[Rook]()
    val bishopsOnBoard = ListBuffer[Bishop]()
    val offBoard       = ListBuffer[Entry]()
    var round: Int     = 0
    var nextId: Int    = 0

    val changes: Iterator[Either[String, BoardCmd]] = snaps
      .sliding(2, 2)
      .map { case List(prev, next) => BoardCmdSolver.solve(prev, next) }

    // early-finish drop-while trick, explained in LongBitOps
    val dropAsWeGo: Iterator[Either[String, BoardCmd]] = changes.dropWhile {
      case Right(RookAdded(at)) =>
        val rat = RoundXY(round, at)
        rooksOnBoard.prepend(Rook(nextId, rat, rat)); round += 1; nextId += 1; true
      case Right(BishopAdded(at)) =>
        val rat = RoundXY(round, at)
        bishopsOnBoard.prepend(Bishop(nextId, rat, rat)); round += 1; nextId += 1; true
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
  private def findByLastAt[E <: Entry](at: Int, buffer: ListBuffer[E]): Option[(Int, E)] =
    findIndexByLastAt(at, buffer).map(idx => (idx, buffer(idx)))

  private def findIndexByLastAt[E <: Entry](at: Int, buffer: ListBuffer[E]): Option[Int] = {
    val idx = buffer.indexWhere(_.lastAt.at == at)
    Option.when(idx >= 0)(idx)
  }

}

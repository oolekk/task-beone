package domain

import util.BoardRender
import util.LongBitOps._
import domain.GameSnap._
import domain.LogEntry.{Bishop, Rook}

import scala.util.Random.nextLong

case class Game private (
  id: String,
  round: Int,
  snaps: List[GameSnap] = List(GameSnap.empty),
  logs: GameLog = GameLog.empty,
  pending: List[BoardCmd] = Nil
)

object Game {

  def init: Game = Game(nextLong.hexString, snaps = List(GameSnap.empty), round = 0)

  def rand: Game = {
    val snap = GameSnap.gen()
    val logs = GameLog.fresh(snap)
    val pending = logs.on.headOption.map {
      case Rook(_, firstAt, _, _)   => RookAdded(firstAt.at)
      case Bishop(_, firstAt, _, _) => BishopAdded(firstAt.at)
    }.toList
    Game(nextLong.hexString, round = 1, snaps = List(snap), logs = logs, pending = pending)
  }

  implicit class GameOps(game: Game) {

    def snap: GameSnap   = game.snaps.headOption.getOrElse(GameSnap.empty)
    def noopInfo: String = s"$gameRound, last saved ${game.pending.size} rounds ago \n$boardInfo"
    def loadInfo: String = s"Loaded $noopInfo"

    def descAllInfo: String =
      game.logs.describeAll(game.round).mkString("\n")

    def descOneInfo(id: Int): String =
      game.logs.describeOne(id, game.round).getOrElse("NO SUCH ID")

    private def boardInfo: String = BoardRender.asGameBoard(game.snap)

    // we print after move, prev round, but it's next round waiting for move, so one too much
    private def gameRound: String = s"GameId: ${game.id} round: ${game.round - 1}"

    def applyAddRook(xy: (Int, Int)): Either[String, Game] = game.snap
      .addRook(xy)
      .map(nextSnap =>
        game.copy(
          round = game.round + 1,
          snaps = nextSnap :: game.snaps,
          logs = game.logs.applyAddRook(game.round, xy),
          pending = RookAdded(xy) :: game.pending
        )
      )

    def applyAddBishop(xy: (Int, Int)): Either[String, Game] = game.snap
      .addBishop(xy)
      .map(nextSnap =>
        game.copy(
          round = game.round + 1,
          snaps = nextSnap :: game.snaps,
          logs = game.logs.applyAddBishop(game.round, xy),
          pending = BishopAdded(xy) :: game.pending
        )
      )

    def applyMove(xyOld: (Int, Int), xyNew: (Int, Int)): Either[String, Game] =
      applyMoveRook(xyOld, xyNew) orElse applyMoveBishop(xyOld, xyNew)

    private def applyMoveBishop(xyOld: (Int, Int), xyNew: (Int, Int)) =
      game.snap.moveBishop(xyOld, xyNew).map { nextSnap =>
        val nextLogs = game.logs.applyMovePiece(game.round, xyOld, xyNew)
        val cmds     = BishopMoved(xyOld, xyNew) :: game.pending
        game.copy(round = game.round + 1, snaps = nextSnap :: game.snaps, logs = nextLogs, pending = cmds)
      }

    private def applyMoveRook(xyOld: (Int, Int), xyNew: (Int, Int)) = {
      game.snap.moveRook(xyOld, xyNew).map { nextSnap =>
        val nextLogs = game.logs.applyMovePiece(game.round, xyOld, xyNew)
        val cmds     = RookMoved(xyOld, xyNew) :: game.pending
        game.copy(round = game.round + 1, snaps = nextSnap :: game.snaps, logs = nextLogs, pending = cmds)
      }
    }

    def applyTake(xy: (Int, Int)): Either[String, Game] =
      applyTakeRook(xy) orElse applyTakeBishop(xy) orElse Left(GameSnap.Error.PIECE_NOT_THERE_MSG)

    private def applyTakeRook(xy: (Int, Int)): Either[String, Game] =
      game.snap.takeRook(xy).map { nextSnap =>
        game.copy(
          round = game.round + 1,
          snaps = nextSnap :: game.snaps,
          logs = game.logs.applyTakePiece(game.round, xy),
          pending = RookTaken(xy) :: game.pending
        )
      }

    private def applyTakeBishop(xy: (Int, Int)): Either[String, Game] =
      game.snap.takeBishop(xy).map { nextSnap =>
        game.copy(
          round = game.round + 1,
          snaps = nextSnap :: game.snaps,
          logs = game.logs.applyTakePiece(game.round, xy),
          pending = BishopTaken(xy) :: game.pending
        )
      }
  }

}

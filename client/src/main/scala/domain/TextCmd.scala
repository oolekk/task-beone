package domain

import zio._
import domain.GameSnap._
import util.HttpUtil

object TextCmd {

  sealed trait TextCmd
  sealed trait Update // mark commands which update the game board

  case object Exit                     extends TextCmd
  case object Noop                     extends TextCmd
  case object Rand                     extends TextCmd
  case object GameInfo                 extends TextCmd
  case class PieceInfo(id: Int)        extends TextCmd
  case class Load(gameId: String = "") extends TextCmd

  case class Take(from: (Int, Int))                 extends TextCmd with Update
  case class AddRook(at: (Int, Int))                extends TextCmd with Update
  case class AddBishop(at: (Int, Int))              extends TextCmd with Update
  case class Move(from: (Int, Int), to: (Int, Int)) extends TextCmd with Update

  def applyCommand(cmd: TextCmd, game: Game): ZIO[ZIOAppArgs with Scope, String, Game] = cmd match {
    case c: Update => ZIO.fromEither(applyUpdate(c, game))
    case Load(gameId) =>
      HttpUtil.load(gameId).flatMap { snaps =>
        ZIO.fromEither(replay(gameId, snaps).map(_.copy(pending = Nil)))
      }
    case Rand => ZIO.fromEither(Right(Game.rand))
    case _    => ZIO.fromEither(Right(game))
  }

  private def applyUpdate(cmd: Update, game: Game): Either[String, Game] = cmd match {
    case AddRook(xy)        => game.applyAddRook(xy)
    case AddBishop(xy)      => game.applyAddBishop(xy)
    case Take(xy)           => game.applyTake(xy)
    case Move(xyOld, xyNew) => game.applyMove(xyOld, xyNew)
  }

  private def replay(gameId: String, snaps: List[GameSnap]): Either[String, Game] = {
    val logs = snaps.headOption.map(GameLog.fresh).getOrElse(GameLog.empty)
    snaps
      .sliding(2, 1)
      .collect { case List(prev, next) => BoardCmd.resolve(prev, next) }
      .takeWhile(_.isRight)
      .foldLeft(Option(Game(gameId, 1, snaps, logs)).toRight("Failed to reload!")) { (game, cmd) =>
        cmd match {
          case Left(msg)                    => Left(msg)
          case Right(RookMoved(from, to))   => game.flatMap(_.applyMove(asXY(from), asXY(to)))
          case Right(RookAdded(at))         => game.flatMap(_.applyAddRook(asXY(at)))
          case Right(RookTaken(from))       => game.flatMap(_.applyTake(asXY(from)))
          case Right(BishopMoved(from, to)) => game.flatMap(_.applyMove(asXY(from), asXY(to)))
          case Right(BishopAdded(at))       => game.flatMap(_.applyAddBishop(asXY(at)))
          case Right(BishopTaken(from))     => game.flatMap(_.applyTake(asXY(from)))
        }
      }
  }

}

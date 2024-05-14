package domain

import GameSnap._
import util.HttpUtil
import _root_.zio.ZIO

object TextCmd {

  val PROMPT                      = ">"
  val COMMAND_PROMPT_WELCOME      = "TYPE COMMANDS AFTER PROMPT"
  private val INVALID_COMMAND_MSG = "Invalid command!"

  sealed trait TextCmd
  sealed trait Update // mark commands which update the game board

  case object Exit                     extends TextCmd
  case object Noop                     extends TextCmd
  case object Rand                     extends TextCmd
  case object Save                     extends TextCmd
  case class Load(gameId: String = "") extends TextCmd
  case object GameInfo                 extends TextCmd
  case class PieceInfo(id: Int)        extends TextCmd

  case class Take(from: (Int, Int))                 extends TextCmd with Update
  case class AddRook(at: (Int, Int))                extends TextCmd with Update
  case class AddBishop(at: (Int, Int))              extends TextCmd with Update
  case class Move(from: (Int, Int), to: (Int, Int)) extends TextCmd with Update

  def applyToGame(cmd: TextCmd, game: Game) = cmd match {
    case c: Update => ZIO.fromEither(applyUpdate(c, game))
    case Load(gameId) =>
      for {snaps <- HttpUtil.load(gameId); size = snaps.size} yield Game(gameId, size, size, snaps.reverse)
    case Save                    => for { savedAt <- HttpUtil.save(game)} yield game.copy(saved = savedAt)
    case Rand                    => ZIO.fromEither(Right(Game.rand))
    case GameInfo | PieceInfo(_) => ZIO.fromEither(game.updateLog)
    case _                       => ZIO.fromEither(Right(game))
  }

  def applyUpdate(cmd: Update, game: Game): Either[String, Game] = cmd match {
    case AddBishop(xy) =>
      game.snap.addBishop(xy).map(nextSnap => game.copy(round = game.round + 1, snaps = nextSnap :: game.snaps))
    case AddRook(xy) =>
      game.snap.addRook(xy).map(nextSnap => game.copy(round = game.round + 1, snaps = nextSnap :: game.snaps))
    case Move(xyOld, xyNew) =>
      game.snap
        .movePiece(xyOld, xyNew)
        .map(nextSnap => game.copy(round = game.round + 1, snaps = nextSnap :: game.snaps))
    case Take(xy) =>
      game.snap.takePiece(xy).map { nextSnap =>
        game.copy(round = game.round + 1, snaps = nextSnap :: game.snaps)
      }
  }

  def parse(text: String): Either[String, TextCmd] = {
    text.takeWhile(!_.isWhitespace).toLowerCase.trim match {
      case _ if text.isBlank            => Right(Noop)
      case "exit"                       => Right(Exit)
      case "rand"                       => Right(Rand)
      case t @ "save" if t == text.trim => Right(Save)
      case "load"                       => Right(Load(text.drop(4).trim))
      case "r" | "new rook" | "add rook" | "put rook" =>
        parseXY(text, 2).map { case List(x, y) => AddRook(x, y) }
      case "b" | "new bishop" | "add bishop" | "put bishop" =>
        parseXY(text, 2).map { case List(x, y) => AddBishop(x, y) }
      case "m" | "mv" | "mov" | "move" =>
        parseXY(text, 4).map { case List(x1, y1, x2, y2) => Move((x1, y1), (x2, y2)) }
      case "t" | "take" | "d" | "delete" | "del" | "rm" | "remove" =>
        parseXY(text, 2).map { case List(x, y) => Take(x, y) }
      case t @ "info" =>
        if (t == text.trim) Right(GameInfo)
        else parseXY(text, 1).map { case List(id) => PieceInfo(id) }
      case _ => Left(TextCmd.INVALID_COMMAND_MSG)
    }
  }

  private def parseXY(text: String, n: Int, min: Int = 0, max: Int = 7): Either[String, List[Int]] = {
    val digits = text.filter(_.isDigit).map(d => ("" + d).toInt)
    Either.cond(
      digits.length == n && digits.forall(d => d >= min && d <= max),
      digits.toList,
      Error.NO_SUCH_FIELD_MSG
    )
  }

}

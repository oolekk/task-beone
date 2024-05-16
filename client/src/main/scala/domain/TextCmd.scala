package domain

import zio._
import domain.GameSnap.{asXY, _}
import domain.LogEntry._
import util.HttpUtil

object TextCmd {

  def parse(text: String): Either[String, TextCmd] = {
    text.takeWhile(!_.isWhitespace).toLowerCase.trim match {
      case _ if text.isBlank            => Right(Noop)
      case "exit"                       => Right(Exit)
      case "rand"                       => Right(Rand)
      case t @ "rand" if t == text.trim => Right(Rand)
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

  val PROMPT                      = ">"
  val COMMAND_PROMPT_WELCOME      = "TYPE COMMANDS AFTER PROMPT"
  private val INVALID_COMMAND_MSG = "Invalid command!"

  def applyCommand(cmd: TextCmd, game: Game): ZIO[ZIOAppArgs with Scope, String, Game] = cmd match {
    case c: Update => ZIO.fromEither(applyUpdate(c, game))
    case Load(gameId) =>
      HttpUtil.load(gameId).flatMap { snaps =>
        ZIO.fromEither(replay(gameId, snaps))
      }
    case Rand => ZIO.fromEither(Right(Game.rand))
    case _    => ZIO.fromEither(Right(game))
  }

  private def applyUpdate(cmd: Update, game: Game): Either[String, Game] = cmd match {
    case AddRook(xy)        => applyAddRook(game, xy)
    case AddBishop(xy)      => applyAddBishop(game, xy)
    case Take(xy)           => applyTake(game, xy)
    case Move(xyOld, xyNew) => applyMove(game, xyOld, xyNew)
  }

  private def replay(gameId: String, snaps: List[GameSnap]): Either[String, Game] = {
    Option(Game(gameId)).toRight("")
    (GameSnap.empty :: snaps)
      .sliding(2, 1)
      .map { v => println(v); v }
      .collect { case List(prev, next) => BoardCmd.resolve(prev, next) }
      .map { v => println(v); v }
      .takeWhile(_.isRight)
      .foldLeft(Option(Game(gameId)).toRight("Failed to reload!")) { (g, cmd) =>
        cmd match {
          case Left(msg)                    => Left(msg)
          case Right(RookMoved(from, to))   => g.flatMap(game => applyMoveRook(game, asXY(from), asXY(to)))
          case Right(RookAdded(at))         => g.flatMap(game => applyAddRook(game, asXY(at)))
          case Right(RookTaken(from))       => g.flatMap(game => applyTakeRook(game, asXY(from)))
          case Right(BishopMoved(from, to)) => g.flatMap(game => applyMoveBishop(game, asXY(from), asXY(to)))
          case Right(BishopAdded(at))       => g.flatMap(game => applyAddBishop(game, asXY(at)))
          case Right(BishopTaken(from))     => g.flatMap(game => applyTakeBishop(game, asXY(from)))
        }
      }
  }

  private def applyAddRook(game: Game, xy: (Int, Int)) =
    for {
      nextSnap <- game.snap.addRook(xy)
      rat      = RoundXY(game.round, xy)
      nextLogs = GameLog(Rook(game.nextId, rat, rat, List(xy)) :: game.logs.on, game.logs.off)
      cmds     = RookAdded(xy) :: game.pending
      nextGame = game.copy(round = game.round + 1, snaps = nextSnap :: game.snaps, logs = nextLogs, pending = cmds)
    } yield nextGame

  private def applyAddBishop(game: Game, xy: (Int, Int)) =
    for {
      nextSnap <- game.snap.addBishop(xy)
      rat      = RoundXY(game.round, xy)
      nextLogs = GameLog(Bishop(game.nextId, rat, rat, List(xy)) :: game.logs.on, game.logs.off)
      cmds     = BishopAdded(xy) :: game.pending
      nextGame = game.copy(round = game.round + 1, snaps = nextSnap :: game.snaps, logs = nextLogs, pending = cmds)
    } yield nextGame

  private def applyMove(game: Game, xyOld: (Int, Int), xyNew: (Int, Int)) =
    applyMoveRook(game, xyOld, xyNew) orElse applyMoveBishop(game, xyOld, xyNew)

  private def applyMoveBishop(game: Game, xyOld: (Int, Int), xyNew: (Int, Int)) =
    game.snap.moveBishop(xyOld, xyNew).map { nextSnap =>
      val logIdx            = game.logs.on.indexWhere(_.lastAt.at == rawIndex(xyOld))
      val (pre, log :: aft) = game.logs.on.splitAt(logIdx)
      val nextLog           = Bishop(log.id, log.firstAt, RoundXY(game.round, xyNew), xyNew :: log.moves)
      val nextLogs          = GameLog(pre ::: nextLog :: aft, game.logs.off)
      val cmds              = BishopMoved(xyOld, xyNew) :: game.pending
      game.copy(round = game.round + 1, snaps = nextSnap :: game.snaps, logs = nextLogs, pending = cmds)
    }

  private def applyMoveRook(game: Game, xyOld: (Int, Int), xyNew: (Int, Int)) =
    game.snap.moveRook(xyOld, xyNew).map { nextSnap =>
      val logIdx            = game.logs.on.indexWhere(_.lastAt.at == rawIndex(xyOld))
      val (pre, log :: aft) = game.logs.on.splitAt(logIdx)
      val nextLog           = Rook(log.id, log.firstAt, RoundXY(game.round, xyNew), xyNew :: log.moves)
      val nextLogs          = GameLog(pre ::: nextLog :: aft, game.logs.off)
      val cmds              = RookMoved(xyOld, xyNew) :: game.pending
      game.copy(round = game.round + 1, snaps = nextSnap :: game.snaps, logs = nextLogs, pending = cmds)
    }

  private def applyTake(game: Game, xy: (Int, Int)): Either[String, Game] =
    applyTakeRook(game, xy) orElse applyTakeBishop(game, xy) orElse Left(GameSnap.Error.PIECE_NOT_THERE_MSG)

  private def applyTakeRook(game: Game, xy: (Int, Int)): Either[String, Game] =
    game.snap.takeRook(xy).map { nextSnap =>
      val logIdx            = game.logs.on.indexWhere(_.lastAt.at == rawIndex(xy))
      val (pre, log :: aft) = game.logs.on.splitAt(logIdx)
      val nextLog           = Rook(log.id, log.firstAt, RoundXY(game.round, xy), log.moves)
      val nextLogs          = GameLog(pre ::: aft, nextLog :: game.logs.off)
      val cmds              = RookTaken(xy) :: game.pending
      game.copy(round = game.round + 1, snaps = nextSnap :: game.snaps, logs = nextLogs, pending = cmds)
    }

  private def applyTakeBishop(game: Game, xy: (Int, Int)): Either[String, Game] =
    game.snap.takeBishop(xy).map { nextSnap =>
      val logIdx            = game.logs.on.indexWhere(_.lastAt.at == rawIndex(xy))
      val (pre, log :: aft) = game.logs.on.splitAt(logIdx)
      val nextLog           = Bishop(log.id, log.firstAt, RoundXY(game.round, xy), log.moves)
      val nextLogs          = GameLog(pre ::: aft, nextLog :: game.logs.off)
      val cmds              = BishopTaken(xy) :: game.pending
      game.copy(round = game.round + 1, snaps = nextSnap :: game.snaps, logs = nextLogs, pending = cmds)
    }

  private def parseXY(text: String, n: Int, min: Int = 0, max: Int = 7): Either[String, List[Int]] = {
    val digits = text.filter(_.isDigit).map(d => ("" + d).toInt)
    if (digits.length != n) Left("Wrong argument count!")
    else
      Either.cond(
        digits.forall(d => d >= min && d <= max),
        digits.toList,
        Error.NO_SUCH_FIELD_MSG
      )
  }

}

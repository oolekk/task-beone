package domain

import domain.GameSnap.XY_OUT_OF_RANGE_MSG

object TextCmd {

  val PROMPT                      = ">"
  val COMMAND_PROMPT_WELCOME      = "TYPE COMMANDS AFTER PROMPT"
  private val INVALID_COMMAND_MSG = "Invalid command!"

  sealed trait TextCmd
  sealed trait Update // mark commands which update the game board

  case object Exit extends TextCmd
  case object Noop extends TextCmd
  case object Rand extends TextCmd
  case object Info extends TextCmd // show game info

  case class Play(gameId: String = "") extends TextCmd
  case class Gone(field: (Int, Int))   extends TextCmd

  case class AddRook(at: (Int, Int))   extends TextCmd with Update
  case class AddBishop(at: (Int, Int)) extends TextCmd with Update
  case class Take(from: (Int, Int))    extends TextCmd with Update

  case class Move(from: (Int, Int), to: (Int, Int)) extends TextCmd with Update

  def applyToGame(cmd: TextCmd, game: Game): Either[String, Game] = cmd match {
    case c: Update    => applyUpdate(c, game)
    case Play(gameId) => restoreGame(gameId)
    case _            => Right(game)
  }

  def restoreGame(gameId: String): Either[String, Game] = {
    // should restore game if id exists, simplified for now
    Right(Game.empty(gameId))
  }

  def applyUpdate(cmd: Update, game: Game): Either[String, Game] = cmd match {
    case AddBishop(xy) =>
      game.moved.addBishop(xy).map(nextSnap => game.copy(round = game.round + 1, moved = nextSnap))
    case AddRook(xy) =>
      game.moved.addRook(xy).map(nextSnap => game.copy(round = game.round + 1, moved = nextSnap))
    case Move(xyOld, xyNew) =>
      game.moved.movePiece(xyOld, xyNew).map(nextSnap => game.copy(round = game.round + 1, moved = nextSnap))
    case Take(xy) =>
      game.moved.takePiece(xy).map { nextSnap =>
        val nextGone = if (game.moved.isRook(xy)) {
          game.taken.forceRook(xy)
        } else if (game.moved.isBishop(xy)) {
          game.taken.forceBishop(xy)
        } else game.taken // shouldn't really happen, since update was successful either rook or bishop was taken
        game.copy(round = game.round + 1, moved = nextSnap, taken = nextGone)
      }
  }

  def parse(text: String): Either[String, TextCmd] = {
    text.takeWhile(!_.isWhitespace).toLowerCase.trim match {
      case _ if text.isBlank => Right(Noop)
      case "info"            => Right(Info)
      case "exit"            => Right(Exit)
      case "rand"            => Right(Rand)
      case "gone" | "last" =>
        parseDigits(text, 2).map { case List(x, y) => Gone(x, y) }
      case "play" => Right(Play(text.drop(4).trim))
      case "r" | "new rook" | "add rook" | "put rook" =>
        parseDigits(text, 2).map { case List(x, y) => AddRook(x, y) }
      case "b" | "new bishop" | "add bishop" | "put bishop" =>
        parseDigits(text, 2).map { case List(x, y) => AddBishop(x, y) }
      case "m" | "mv" | "mov" | "move" =>
        parseDigits(text, 4).map { case List(x1, y1, x2, y2) => Move((x1, y1), (x2, y2)) }
      case "t" | "take" | "d" | "delete" | "del" | "rm" | "remove" =>
        parseDigits(text, 2).map { case List(x, y) => Take(x, y) }
      case _ => Left(TextCmd.INVALID_COMMAND_MSG)
    }
  }

  private def parseDigits(text: String, n: Int, min: Int = 0, max: Int = 7): Either[String, List[Int]] = {
    val digits = text.filter(_.isDigit).map(d => ("" + d).toInt)
    Either.cond(
      digits.length == n && digits.forall(d => d >= min && d <= max),
      digits.toList,
      XY_OUT_OF_RANGE_MSG
    )
  }

}

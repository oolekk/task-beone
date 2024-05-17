package util

import domain.Game
import domain.GameSnap.{Error, _}
import domain.TextCmd._

object CmdParser {

  private val INVALID_COMMAND_MSG      = "Invalid command!"
  private val WRONG_ARGUMENT_COUNT_MSG = "Wrong argument count!"

  private val EXIT_TXT    = "exit"
  private val INFO_TXT    = "info"
  private val RAND_TXT    = "rand"
  private val LOAD_TXT    = "load"
  def parse(text: String, game: Game): Either[String, TextCmd] = {
    text.takeWhile(!_.isWhitespace).toLowerCase.trim match {
      case _ if text.isBlank                => Right(Noop)
      case num if num.toIntOption.isDefined => byIdCmd(game, text)
      case EXIT_TXT                         => Right(Exit)
      case RAND_TXT                         => Right(Rand)
      case "restart" | "reset"              => Right(Restart)
      case LOAD_TXT                         => Right(Load(text.drop(4).trim))
      case txt if isNewRookTxt(txt) =>
        parseXY(text, 2).map { case List(x, y) => AddRook(x, y) }
      case txt if isNewBishopTxt(txt) =>
        parseXY(text, 2).map { case List(x, y) => AddBishop(x, y) }
      case txt if isMoveTxt(txt) =>
        parseXY(text, 4).map { case List(x1, y1, x2, y2) => Move((x1, y1), (x2, y2)) }
      case txt if isTakeTxt(txt) =>
        parseXY(text, 2).map { case List(x, y) => Take(x, y) }
      case INFO_TXT =>
        if (INFO_TXT == text.trim) Right(GameInfo)
        else parseXY(text, 1).map { case List(id) => PieceInfo(id) }
      case _ => Left(INVALID_COMMAND_MSG)
    }
  }

  private def byIdCmd(game: Game, text: String): Either[String, TextCmd] = {
    val parts = text.split(' ').map(_.trim).filter(_.nonEmpty).toList
    for {
      id <- parts.headOption.flatMap(_.toIntOption)
    } yield parts match {
      case id :: Nil             => Right(PieceInfo(id.toInt))
      case id :: INFO_TXT :: Nil => Right(PieceInfo(id.toInt))
      case _ :: cmd :: tail if isMoveTxt(cmd) =>
        game.logs
          .findAtById(id)
          .flatMap(at => parseXY(tail.mkString(""), 2).map { case List(x, y) => Move(asXY(at), (x, y)) })
      case _ :: cmd :: Nil if isTakeTxt(cmd) =>
        game.logs.findAtById(id).flatMap(at => Right(Take(asXY(at))))
      case _ => Left(INVALID_COMMAND_MSG)
    }
  }.getOrElse(Left(INVALID_COMMAND_MSG))

  private val moveTexts      = List("m", "mv", "mov", "move")
  private val takeTexts      = List("t", "take", "d", "delete", "del", "rm", "rmv", "remove")
  private val newRookTexts   = List("r", "new rook", "add rook", "put rook")
  private val newBishopTexts = List("b", "new bishop", "add bishop", "put bishop")

  private def isMoveTxt(txt: String)      = moveTexts.contains(txt)
  private def isTakeTxt(txt: String)      = takeTexts.contains(txt)
  private def isNewRookTxt(txt: String)   = newRookTexts.contains(txt)
  private def isNewBishopTxt(txt: String) = newBishopTexts.contains(txt)

  private def parseXY(text: String, n: Int, min: Int = 0, max: Int = 7): Either[String, List[Int]] = {
    val digits = text.filter(_.isDigit).map(d => ("" + d).toInt)
    if (digits.length != n) Left(WRONG_ARGUMENT_COUNT_MSG)
    else
      Either.cond(
        digits.forall(d => d >= min && d <= max),
        digits.toList,
        Error.NO_SUCH_XY_MSG
      )
  }

}

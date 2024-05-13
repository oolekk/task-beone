package app

import domain.TextCmd._
import domain._
import util.GameSnapUtil
import zio.Console._
import zio._
import util.LongBitOps._
import scala.util.Random.nextLong

object TestClient extends ZIOAppDefault {

  def run: Task[Unit] = replInit(Game.empty(nextLong.hexString))

  def replInit(game: Game): ZIO[Any, Throwable, Unit] =
    for {
      _ <- print(s"$COMMAND_PROMPT_WELCOME\n$PROMPT")
      _ <- repl(game)
    } yield ()

  def replRandBoard(): ZIO[Any, Throwable, Unit] =
    for {
      randSnap <- ZIO.from(GameSnap.gen())
      _        <- printLine(GameSnapUtil.asGameBoard(randSnap))
      _        <- print(PROMPT)
      _        <- repl(Game.empty.copy(snaps = List(randSnap)))
    } yield ()

  def repl(game: Game): ZIO[Any, Throwable, Unit] = {
    for {
      cmdOpt <- readLine.map(TextCmd.parse)
      _ <- ZIO.unless(cmdOpt.contains(TextCmd.Exit)) {
        val newGameZio = for {
          _        <- print(cmdOpt.map(_.toString + "\n").getOrElse(""))
          cmd      <- ZIO.fromEither(cmdOpt)
          nextGame <- TextCmd.applyToGame(cmd, game)
          _ <- cmdOpt match {
            case Right(Load(gameId)) => printLine(s"LOAD GameId: $gameId round: ${game.round}")
            case Right(Save)         => printLine(s"SAVE GameId: ${game.id} round: ${game.round}")
            case Right(Rand)         => replRandBoard()
            case Right(GameInfo) =>
              GameLog
                .replay(game.id, game.snaps)
                .fold(
                  err => printLine(err),
                  log => { printLine("Game:" + game.id + "\n" + log.describeAll.mkString("\n")) }
                )
            case Right(PieceInfo(id)) =>
              GameLog
                .replay(game.id, game.snaps)
                .fold(err => printLine(err), log => printLine(log.describe(id).getOrElse("NO SUCH ID")))
            case _ => printLine(GameSnapUtil.asGameBoard(nextGame.snap))
          }
          _ <- print(PROMPT)
        } yield nextGame
        newGameZio.foldZIO(_ => repl(game), newGame => repl(newGame))
      }
    } yield ()
  }

}

package app

import domain.TextCmd._
import domain.{Game, GameLog, GameSnap, TextCmd}
import util.GameSnapUtil
import zio.Console._
import zio._

object TestClient extends ZIOAppDefault {

  def run: Task[Unit] = replInit(Game.empty)

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

  def repl(game: Game): ZIO[Any, Throwable, Unit] =
    for {
      cmdOpt <- readLine.map(TextCmd.parse)
      _ <- ZIO.unless(cmdOpt.contains(TextCmd.Exit)) {
        val nextGameZio = for {
          _ <- print(cmdOpt.map(_.toString + "\n").getOrElse(""))
          nextGame = cmdOpt.flatMap(cmd => TextCmd.applyToGame(cmd, game))
          _ <- cmdOpt match {
            case Right(Load(gameId)) => printLine(s"LOAD GameId: ${gameId} round: ${game.round}")
            case Right(Save)         => printLine(s"SAVE GameId: ${game.id} round: ${game.round}")
            case Right(Rand)         => replRandBoard()
            case Right(GameInfo) =>
              GameLog
                .replay(game.id, game.snaps)
                .fold(err => printLine(err), log => printLine(log.describeAll.mkString("\n")))
            case Right(PieceInfo(id)) =>
              GameLog
                .replay(game.id, game.snaps)
                .fold(err => printLine(err), log => printLine(log.describe(id).getOrElse("NO SUCH ID")))
            case _ =>
              nextGame match {
                case Left(msg) => printLine(msg)
                case Right(game) =>
                  printLine(GameSnapUtil.asGameBoard(game.snap))
              }
          }
          _ <- print(PROMPT)
        } yield nextGame
        nextGameZio.flatMap { nextGame => nextGame.fold(_ => repl(game), game => repl(game)) }
      }
    } yield ()

}

package app

import domain.TextCmd._
import domain._
import util.HttpUtil
import zio.Console._
import zio._

object GameApp extends ZIOAppDefault {

  def run: ZIO[ZIOAppArgs & Scope, Throwable, Unit] = replInit(Game.init)

  private def replInit(game: Game): ZIO[ZIOAppArgs & Scope, Throwable, Unit] =
    for {
      _ <- print(s"$COMMAND_PROMPT_WELCOME\n$PROMPT")
      _ <- repl(game)
    } yield ()

  private def repl(game: Game): ZIO[ZIOAppArgs & Scope, Throwable, Unit] = {
    for {
      cmdOpt <- readLine.map(TextCmd.parse)
      _ <- ZIO.unless(cmdOpt.contains(TextCmd.Exit)) {
        for {
          _   <- print(cmdOpt.map(_.toString + "\n").getOrElse(""))
          cmd <- ZIO.fromEither(cmdOpt).tapError(err => printLine(err)) orElse ZIO.succeed(Noop)
          next <- TextCmd
            .applyCommand(cmd, game)
            .foldZIO(err => printLine(err).as(game), next => savedOrSame(next))
          _ <- cmd match {
            case Load(_)       => printLine(next.loadInfo)
            case GameInfo      => printLine(next.descAllInfo)
            case PieceInfo(id) => printLine(next.descOneInfo(id))
            case _             => printLine(next.noopInfo)
          }
          _ <- print(PROMPT)
          _ <- repl(next)
        } yield ()
      }
    } yield ()
  }

  private def savedOrSame(game: Game): URIO[ZIOAppArgs & Scope, Game] = HttpUtil
    .push(game)
    .flatMap(diffCount =>
      if (diffCount < 0) {
        ZIO.fail(s"Change not accepted, ${-diffCount} items discarded!" + diffCount)
      } else {
        ZIO.succeed(game.copy(pending = Nil))
      }
    )
    .tapError(err => printLine(err))
    .orElseSucceed(game)

}

package app

import domain.TextCmd._
import domain._
import zio.Console._
import zio._

object GameApp extends ZIOAppDefault {

  def run: ZIO[ZIOAppArgs & Scope, Throwable, Unit] = replInit(Game.init)

  def replInit(game: Game): ZIO[ZIOAppArgs & Scope, Throwable, Unit] =
    for {
      _ <- print(s"$COMMAND_PROMPT_WELCOME\n$PROMPT")
      _ <- repl(game)
    } yield ()

  def repl(game: Game): ZIO[ZIOAppArgs & Scope, Throwable, Unit] = {
    for {
      cmdOpt <- readLine.map(TextCmd.parse)
      _ <- ZIO.unless(cmdOpt.contains(TextCmd.Exit)) {
        for {
          _   <- print(cmdOpt.map(_.toString + "\n").getOrElse(""))
          cmd <- ZIO.fromEither(cmdOpt) orElse ZIO.succeed(Noop)
          app = TextCmd.applyToGame(cmd, game)
          _    <- app.flip.forEachZIO(printLine(_))
          next <- TextCmd.applyToGame(cmd, game) orElse ZIO.succeed(game)
          _ <- cmdOpt match {
            case Right(Load(_))       => printLine(next.loadInfo)
            case Right(Save)          => printLine(next.saveInfo)
            case Right(GameInfo)      => printLine(next.descAllInfo)
            case Right(PieceInfo(id)) => printLine(next.descOneInfo(id))
            case _                    => printLine(next.noopInfo)
          }
          _ <- print(PROMPT)
          _ <- repl(next)
        } yield ()
      }
    } yield ()
  }

}

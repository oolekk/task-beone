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
          _    <- print(cmdOpt.map(_.toString + "\n").getOrElse(""))
          cmd  <- ZIO.fromEither(cmdOpt) orElse ZIO.succeed(Noop)
          next <- TextCmd.applyToGame(cmd, game) orElse ZIO.succeed(game)
          nextOrSaved <- cmdOpt match {
            case Right(_: Update) =>
              for {
                update <- HttpUtil.push(next).orElseSucceed(-1)
                savedGame = if (update > 0) next.copy(pending = Nil, saved = next.round) else next
                _ <- printLine("Pushed " + savedGame.noopInfo)
              } yield savedGame
            case Right(Load(_))       => for { _ <- printLine(next.loadInfo) } yield next
            case Right(Save)          => for { _ <- printLine(next.saveInfo) } yield next
            case Right(GameInfo)      => for { _ <- printLine(next.descAllInfo) } yield next
            case Right(PieceInfo(id)) => for { _ <- printLine(next.descOneInfo(id)) } yield next
            case _                    => for { _ <- printLine(next.noopInfo) } yield next
          }
          _ <- print(PROMPT)
          _ <- repl(nextOrSaved)
        } yield ()
      }
    } yield ()
  }

}

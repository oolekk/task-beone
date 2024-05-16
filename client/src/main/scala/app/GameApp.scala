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
          // try to apply change to the previous game state, print error and fallback to existing state otherwise
          next <- TextCmd.applyCommand(cmd, game).foldZIO(err => printLine(err).as(game), next => ZIO.succeed(next))
          nextOrSaved <- cmdOpt match {
            case Left(err) => for { _ <- printLine(err) } yield next // inform if can't parse cmd
            case Right(_: Update) =>
              for {
                update <- HttpUtil.push(next).orElseSucceed(-next.pending.size)
                // continue playing even if push failed, push all outstanding moves on next move
                savedGame = if (update > 0) next.copy(pending = Nil, saved = next.round) else next
                _ <- printLine("Pushed " + savedGame.noopInfo)
              } yield savedGame
            case Right(Rand) => // save it only after first move, same like with new empty game
              for {
                randGame <- ZIO.succeed(Game.rand)
                _        <- printLine(randGame.noopInfo)
              } yield randGame
            case Right(Load(_))       => for { _ <- printLine(next.loadInfo) } yield next
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

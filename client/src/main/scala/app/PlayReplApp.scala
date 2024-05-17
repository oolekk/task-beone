package app

import domain.TextCmd._
import domain._
import util.{CmdParser, HttpUtil}
import zio.Console._
import zio._

object PlayReplApp extends ZIOAppDefault {

  def run: ZIO[ZIOAppArgs & Scope, Throwable, Unit] = replInit(Game.init)

  private val PROMPT                 = ">"
  private val COMMAND_PROMPT_WELCOME = "TYPE COMMANDS AFTER PROMPT"
  private def saveNotAcceptedMsg(storedCount: Int) =
    s"Save not accepted, total saved count stays at: ${storedCount.abs}"

  private def replInit(game: Game): ZIO[ZIOAppArgs & Scope, Throwable, Unit] =
    print(s"$COMMAND_PROMPT_WELCOME\n$PROMPT") *> repl(game).unit

  private def repl(game: Game): ZIO[ZIOAppArgs & Scope, Throwable, Unit] = {
    for {
      cmdOpt <- readLine.map(line => CmdParser.parse(line, game))
      _ <-
        if (cmdOpt == Right(Restart)) replInit(Game.init)
        else
          ZIO.unless(cmdOpt.contains(TextCmd.Exit)) {
            for {
              _    <- print(cmdOpt.map(_.toString + "\n").getOrElse(""))
              cmd  <- ZIO.fromEither(cmdOpt).tapError(err => printLine(err)) orElse ZIO.succeed(Noop)
              next <- savedSameOrRandGame(game, cmd)
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

  private def savedSameOrRandGame(game: Game, cmd: TextCmd) = {
    TextCmd
      .applyCommand(cmd, game)
      .foldZIO(
        err => printLine(err).as(game),
        next =>
          cmd match {
            case Rand => ZIO.succeed(next)
            case _    => saveOrFallback(next)
          }
      )
  }

  private def saveOrFallback(game: Game): URIO[ZIOAppArgs & Scope, Game] = HttpUtil
    .push(game)
    .flatMap(storedCount =>
      if (storedCount < 0) ZIO.fail(saveNotAcceptedMsg(storedCount))
      else ZIO.succeed(game.copy(pending = Nil))
    )
    .tapError(err => printLine(err))
    .orElseSucceed(game)

}

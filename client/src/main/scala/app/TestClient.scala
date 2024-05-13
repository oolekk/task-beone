package app

import domain.TextCmd._
import domain.{Game, GameSnap, TextCmd}
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
      _        <- repl(Game.empty.copy(moved = randSnap))
    } yield ()

  def replCheckGone(game: Game, xy: (Int, Int)): ZIO[Any, Throwable, Unit] =
    for {
      _ <- {
        if (game.taken.isBishop(xy)) printLine(s"${xy} was last location of a Bishop")
        else if (game.taken.isRook(xy)) printLine(s"${xy} was last location of a Rook")
        else printLine(s"Nothing was taken away from ${xy}")
      }
      _ <- print(PROMPT)
      _ <- repl(game)
    } yield ()

  def repl(game: Game): ZIO[Any, Throwable, Unit] =
    for {
      cmdOpt <- readLine.map(TextCmd.parse)
      _ <- ZIO.unless(cmdOpt.contains(TextCmd.Exit)) {
        val nextGameZio = for {
          _ <- print(cmdOpt.map(_.toString + "\n").getOrElse(""))
          nextGame = cmdOpt.flatMap(cmd => TextCmd.applyToGame(cmd, game))
          _ <- cmdOpt match {
            case Right(Load(_))  => printLine(s"GameId: ${game.id} round: ${game.round}")
//            case Right(Info)     => c.printLine(s"GameId: ${game.id} round: ${game.round}")
            case Right(Rand)     => replRandBoard()
            case _ =>
              nextGame match {
                case Left(msg) => printLine(msg)
                case Right(g)  => printLine(GameSnapUtil.asGameBoard(g.moved))
              }
          }
          _ <- print(PROMPT)
        } yield nextGame
        nextGameZio.flatMap { nextGame => nextGame.fold(_ => repl(game), g => repl(g)) }
      }
    } yield ()

}

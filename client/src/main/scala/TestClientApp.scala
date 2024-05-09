package com.vowsum.ziodot

import domain.TextCmd._
import domain.{Game, TextCmd}
import util.GameSnapUtil
import zio.Console.ConsoleLive
import zio.{Console, Task, ZIO, ZIOAppDefault}

/*
  This is only test code, made to make testing easier. Please do not use it in production or use it as an example.
 */
object TestClientApp extends ZIOAppDefault {

  def run: Task[Unit] = replInit(ConsoleLive, Game.empty)

  def replInit(c: Console, game: Game): ZIO[Any, Throwable, Unit] =
    for {
      _ <- c.print(s"$COMMAND_PROMPT_WELCOME\n$PROMPT")
      _ <- repl(c, game)
    } yield ()

  def replRandBoard(c: Console): ZIO[Any, Throwable, Unit] =
    for {
      randSnap <- ZIO.from(GameSnapUtil.genRandSnap(1, 1, 5))
      _        <- c.printLine(GameSnapUtil.asGameBoard(randSnap))
      _        <- c.print(PROMPT)
      _        <- repl(c, Game.empty.copy(moved = randSnap))
    } yield ()

  def replCheckGone(c: Console, game: Game, xy: (Int, Int)): ZIO[Any, Throwable, Unit] =
    for {
      _ <- {
        if (game.taken.isBishop(xy)) c.printLine(s"${xy} was last location of a Bishop")
        else if (game.taken.isRook(xy)) c.printLine(s"${xy} was last location of a Rook")
        else c.printLine(s"Nothing was taken away from ${xy}")
      }
      _ <- c.print(PROMPT)
      _ <- repl(c, game)
    } yield ()

  def repl(c: Console, game: Game): ZIO[Any, Throwable, Unit] =
    for {
      line <- c.readLine
      cmdOpt = TextCmd.parse(line)
      _ <- ZIO.unless(cmdOpt.contains(TextCmd.Exit)) {
        val nextGameZio = for {
          _ <- c.print(cmdOpt.map(_.toString + "\n").getOrElse(""))
          nextGame = cmdOpt.flatMap(cmd => TextCmd.applyToGame(cmd, game))
          _ <- cmdOpt match {
            case Right(Play(_))  => c.printLine(s"GameId: ${game.id} round: ${game.round}")
            case Right(Info)     => c.printLine(s"GameId: ${game.id} round: ${game.round}")
            case Right(Rand)     => replRandBoard(c)
            case Right(Gone(xy)) => replCheckGone(c, game, xy)
            case _ =>
              nextGame match {
                case Left(msg) => c.printLine(msg)
                case Right(g)  => c.printLine(GameSnapUtil.asGameBoard(g.moved))
              }
          }
          _ <- c.print(PROMPT)
        } yield nextGame
        nextGameZio.flatMap { nextGame => nextGame.fold(_ => repl(c, game), g => repl(c, g)) }
      }
    } yield ()

}

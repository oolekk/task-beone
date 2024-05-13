package app

import domain.{Game, Hex}
import zio.Console.ConsoleLive
import zio._
import zio.http._

object RestClient extends ZIOAppDefault {

  def save(game: Game): ZIO[ZIOAppArgs & Scope, Throwable, Unit] = {
    for {
      resp <- Client
        .request(Request.get(s"http://localhost:8090/save/${game.id}/${game.round}"))
        .provide(Client.default, Scope.default)
      body <- resp.body.asString
    } yield body
  }

  def run: ZIO[ZIOAppArgs & Scope, Throwable, Unit] = for {
    resp <- Client
      .request(Request.get("http://localhost:8090/status"))
      .provide(Client.default, Scope.default)
    body <- resp.body.asString
    _    <- ConsoleLive.printLine(body)
  } yield ()

}

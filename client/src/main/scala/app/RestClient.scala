package app

import zio.Console.ConsoleLive
import zio._
import zio.http._

object RestClient extends ZIOAppDefault {
  def run: ZIO[ZIOAppArgs & Scope, Throwable, Unit] = for {
    resp <- Client
      .request(Request.get("http://localhost:8090/status"))
      .provide(Client.default, Scope.default)
    body <- resp.body.asString
    _    <- ConsoleLive.printLine(body)
  } yield ()

}

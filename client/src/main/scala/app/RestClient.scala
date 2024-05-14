package app

import domain.{Game, GameSnap, Hex}
import zio.Console.ConsoleLive
import zio._
import zio.http._
import zio.json._

import java.time.temporal.ChronoUnit.SECONDS

object RestClient extends ZIOAppDefault {

  def load(gameId: String): ZIO[ZIOAppArgs & Scope, String, List[GameSnap]] =
    for {
      resp <- Client
        .request(
          Request.get(s"http://localhost:8090/load/$gameId")
        )
        .timeoutFail("Load timed out!")(Duration(3, SECONDS))
        .provide(Client.default, Scope.default)
        .mapError(err => "Load failed to fetch response!")
      str <- resp.body.asString
        .mapError(err => "Load failed to read response!")
      hexList <- ZIO
        .fromEither(Hex.listDecoder.decodeJson(str))
        .mapError(err => "Load failed to decode response!")
      snaps <- ZIO
        .foreach(hexList)(hex => GameSnap.zioFromHex(hex.hex))
        .mapError(err => "Load failed to decode snaps!")
    } yield snaps

  def save(game: Game): ZIO[ZIOAppArgs & Scope, String, Int] = {
    val unsaved = game.snaps.take(game.round - game.saved).reverse
    for {
      resp <- Client
        .request(
          Request.post(
            path = s"http://localhost:8090/save/${game.id}/${game.saved}",
            body = Body.fromString(unsaved.toJson)
          )
        )
        .timeoutFail("Save timed out!")(Duration(3, SECONDS))
        .provide(Client.default, Scope.default)
        .mapError(err => "Save failed to fetch response!")
      body <- resp.body.asString
        .mapError(err => "Save failed to read response!")
      savedAt <- ZIO
        .attempt(body.toInt)
        .mapError(err => "Save failed to decode response!")
    } yield savedAt
  }

  def run: ZIO[ZIOAppArgs & Scope, Throwable, Unit] = for {
    resp <- Client
      .request(Request.get("http://localhost:8090/status"))
      .provide(Client.default, Scope.default)
    body <- resp.body.asString
    _    <- ConsoleLive.printLine(body)
  } yield ()

}

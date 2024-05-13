package app

import domain.{Game, GameSnap, Hex}
import zio.Console.ConsoleLive
import zio._
import zio.http._
import zio.json._

import java.time.temporal.ChronoUnit

object RestClient extends ZIOAppDefault {

  def load(gameId: String): ZIO[Any, String, List[GameSnap]] =
    for {
      resp <- Client
        .request(
          Request.get(s"http://localhost:8090/load/${gameId}")
        )
        .timeoutFail("LOAD TIMED OUT")(Duration(3, ChronoUnit.SECONDS))
        .provide(Client.default, Scope.default)
        .mapError(err => "FAILED TO FETCH")
      str <- resp.body.asString
        .mapError(err => "FAILED TO READ")
      hexList <- ZIO
        .fromEither(Hex.listDecoder.decodeJson(str))
        .mapError(err => "FAILED TO DECODE")
      snaps <- ZIO
        .foreach(hexList)(hex => GameSnap.zioFromHex(hex.hex))
        .mapError(err => "FAILED TO PARSE")
    } yield snaps

  def save(game: Game): ZIO[Any, String, Int] = {
    val unsaved = game.snaps.take(game.round - game.savedAt).reverse
    for {
      resp <- Client
        .request(
          Request.post(
            path = s"http://localhost:8090/save/${game.id}/${game.savedAt}",
            body = Body.fromString(unsaved.toJson)
          )
        )
        .timeoutFail("SAVE TIMED OUT")(Duration(3, ChronoUnit.SECONDS))
        .provide(Client.default, Scope.default)
        .mapError(err => "FAILED TO FETCH ")
      body <- resp.body.asString
        .mapError(err => "FAILED TO READ")
      savedAt <- ZIO
        .attempt(body.toInt)
        .mapError(err => "FAILED TO DECODE")
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

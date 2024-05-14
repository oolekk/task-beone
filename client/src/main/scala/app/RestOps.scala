package app

import domain._
import zio.Console.ConsoleLive
import zio._
import zio.http._

import java.time.temporal.ChronoUnit.SECONDS

object RestOps {

  def load(gameId: String): ZIO[ZIOAppArgs & Scope, String, List[GameSnap]] = {
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
        .fromEither(HexList.decoder.decodeJson(str))
        .mapError(err => "Load failed to decode response!")
      snaps <- ZIO
        .foreach(hexList.hxs)(hex => GameSnap.zioFromHex(hex))
        .mapError(err => "Load failed to decode snaps!")
    } yield snaps
  }

  def save(game: Game): ZIO[ZIOAppArgs & Scope, String, Int] = {
    val unsaved = game.snaps.take(game.round - game.saved).reverse
    val payload = HexList.encoder.encodeJson(HexList(unsaved.map(_.asHexStr))).toString
    for {
      resp <- Client
        .request(
          Request.post(
            path = s"http://localhost:8090/save/${game.id}/${game.saved}",
            body = Body.fromString(payload)
          )
        )
        .timeoutFail("Save timed out!")(Duration(3, SECONDS))
        .provide(Client.default, Scope.default)
        .mapError(err => "Save failed to fetch response!")
      body <- resp.body.asString
        .mapError(err => "Save failed to read response!")
      savedAt <- ZIO
        .attempt(body.toInt)
        .mapError(err => "Save failed to decode response!" + err)
    } yield savedAt
  }

  def status(): ZIO[ZIOAppArgs & Scope, Throwable, Unit] = for {
    resp <- Client
      .request(Request.get("http://localhost:8090/status"))
      .provide(Client.default, Scope.default)
    body <- resp.body.asString
    _    <- ConsoleLive.printLine(body)
  } yield ()

}

package util

import configuration.ClientConfig.configuration
import domain._
import zio._
import zio.http._

import java.time.temporal.ChronoUnit.SECONDS

object HttpUtil {

  def push(game: Game): ZIO[ZIOAppArgs & Scope, String, Int] = {
    val cmds: SnapCmds = SnapCmds(
      game.pending
        .zip(game.snaps.take(game.pending.size))
        .reverse
        .map { case (cmd, snap) => SnapCmd(cmd, snap) }
    )
    val payload = SnapCmds.encoder.encodeJson(cmds).toString
    for {
      resp <- Client
        .request(
          Request.post(
            path = s"${configuration.rest.safePushUrl}/${game.id}/${game.saved}",
            body = Body.fromString(payload)
          )
        )
        .timeoutFail("Update timed out!")(Duration(3, SECONDS))
        .provide(Client.default, Scope.default)
        .orElseFail("Update failed to fetch response!")
      body <- resp.body.asString.orElseFail("Update failed to read response!")
      updatedAt <- ZIO
        .attempt(body.toInt)
        .orElseFail("Update failed to decode response!")
    } yield updatedAt
  }

  def load(gameId: String): ZIO[ZIOAppArgs & Scope, String, List[GameSnap]] = {
    for {
      resp <- Client
        .request(
          Request.get(s"${configuration.rest.loadGameUrl}/$gameId")
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
    val unsaved = game.snaps.take(game.round - game.saved)
    val payload = HexList.encoder.encodeJson(HexList(unsaved.map(_.asHexStr))).toString
    for {
      resp <- Client
        .request(
          Request.post(
            path = s"${configuration.rest.saveGameUrl}/${game.id}/${game.saved}",
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
        .mapError(err => "Save failed to decode response! " + err)
    } yield savedAt
  }

}

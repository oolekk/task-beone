package util

import configuration.ClientConfig.configuration
import domain._
import zio._
import zio.http._

import java.time.temporal.ChronoUnit.SECONDS

object HttpUtil {

  private val TIMEOUT = Duration(3, SECONDS)

  private def timeoutMsg(pre: String)      = s"$pre timed out!"
  private def failedFetchMsg(pre: String)  = s"$pre failed to fetch response!"
  private def failedReadMsg(pre: String)   = s"$pre failed to read response!"
  private def failedDecodeMsg(pre: String) = s"$pre failed to decode response!"

  def push(game: Game): ZIO[ZIOAppArgs & Scope, String, Int] = {
    val cmds: SnapCmds = SnapCmds(
      game.pending
        .zip(game.snaps.take(game.pending.size))
        .reverse
        .map { case (cmd, snap) => SnapCmd(cmd, snap) }
    )
    for {
      resp <- Client
        .request(
          Request.post(
            path = s"${configuration.rest.safePushUrl}/${game.id}/${game.saved}",
            body = Body.fromString(SnapCmds.encoder.encodeJson(cmds).toString)
          )
        )
        .timeoutFail(timeoutMsg("Push"))(TIMEOUT)
        .provide(Client.default, Scope.default)
        .orElseFail(failedFetchMsg("Push"))
      body      <- resp.body.asString.orElseFail(failedReadMsg("Push"))
      updatedAt <- ZIO.attempt(body.toInt).orElseFail(failedDecodeMsg("Push"))
    } yield updatedAt
  }

  def load(gameId: String): ZIO[ZIOAppArgs & Scope, String, List[GameSnap]] = {
    for {
      resp <- Client
        .request(
          Request.get(s"${configuration.rest.loadGameUrl}/$gameId")
        )
        .timeoutFail(timeoutMsg("Load"))(TIMEOUT)
        .provide(Client.default, Scope.default)
        .orElseFail(failedFetchMsg("Load"))
      str     <- resp.body.asString.orElseFail(failedReadMsg("Load"))
      hexList <- ZIO.fromEither(HexList.decoder.decodeJson(str)).orElseFail(failedDecodeMsg("Load HexList"))
      snaps   <- ZIO.foreach(hexList.hxs)(hex => GameSnap.zioFromHex(hex)).orElseFail(failedDecodeMsg("Load GameSnap"))
    } yield snaps
  }

}

package service

import domain.{BishopAdded, CmdAck, GameSnap}
import sttp.apispec.openapi.circe.yaml._
import sttp.tapir.PublicEndpoint
import sttp.tapir.docs.openapi._
import sttp.tapir.generic.auto._
import sttp.tapir.json.zio.jsonBody
import sttp.tapir.server.ziohttp.ZioHttpInterpreter
import sttp.tapir.swagger.SwaggerUI
import sttp.tapir.ztapir._
import util.LongBitOps._
import zio._

import scala.util.Random.nextInt

object RestService extends ZIOAppDefault {

  type RestPoint[In, Out] = PublicEndpoint[In, Unit, Out, Any]

  val healthCheck: RestPoint[Unit, String] =
    endpoint.get.in("health").out(stringBody)
  def healthLogic(): ZIO[Any, Nothing, String] = for {
    aliveMsg <- ZIO.succeed("SERVICE IS UP!")
  } yield aliveMsg

  private val postNextSnapEndpoint: RestPoint[(String, GameSnap), CmdAck] =
    // allows to save next snap after move is made
    // for empty id it will establish new id of new game and save its first snap
    // returns ack with game id, round number and either command applied or error
    endpoint.post.in("post-next-snap" / path[String]).in(jsonBody[GameSnap]).out(jsonBody[CmdAck])

  private def postNextSnapLogic(gameId: String, snap: GameSnap): ZIO[Any, Unit, CmdAck] =
    if ((snap.bishops ^ snap.rooks).count1s == 1) {
      ZIO.succeed(
        CmdAck(gameId, s"$gameId-${nextInt}", Option(BishopAdded(1)).toRight("Will push to kafka here"))
      )
    } else
      ZIO.succeed(
        CmdAck(
          gameId,
          s"$gameId-${nextInt()}",
          Left("""Bishops and Rooks must not overlap, try {  "rs": 1, "bs": 0}""")
        )
      )

  private def getAllGameSnapsEndpoint: RestPoint[String, List[GameSnap]] = {
    // Allows to restore game fully from gameID, all snaps after each move.
    // I won't bother with database, file-per-game named after game ID will do.
    endpoint.get.in("get-all-snaps" / path[String]).out(jsonBody[List[GameSnap]])
  }

  def getAllGameSnapsLogic(gameId: String): ZIO[Any, Unit, List[GameSnap]] =
    ZIO.succeed(List(GameSnap(0, 1), GameSnap(3, 1))) // will read the blob here

  private val appRoutes = List(
    healthCheck.zServerLogic(_ => healthLogic()),
    postNextSnapEndpoint.zServerLogic { case (gameId, snap) => postNextSnapLogic(gameId, snap) },
    getAllGameSnapsEndpoint.zServerLogic { gameId => getAllGameSnapsLogic(gameId) }
  )

  private val openApi = OpenAPIDocsInterpreter()
    .toOpenAPI(appRoutes.map(_.endpoint), "BeOne", "1.0")
  private val swaggerUI = SwaggerUI[Task](openApi.toYaml)

  private val swgHttp = ZioHttpInterpreter().toHttp(swaggerUI)
  private val appHttp = ZioHttpInterpreter().toHttp(appRoutes)
  private val http    = appHttp ++ swgHttp

  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {
    zio.http.Server
      .serve(http)
      .provide(zio.http.Server.defaultWithPort(7090))
      .exitCode
  }

}

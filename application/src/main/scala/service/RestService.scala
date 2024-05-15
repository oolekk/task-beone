package service

import configuration.AppConfig
import domain.{HexList, SnapCmd, SnapCmds}
import sttp.apispec.openapi.circe.yaml._
import sttp.tapir.PublicEndpoint
import sttp.tapir.docs.openapi._
import sttp.tapir.generic.auto._
import sttp.tapir.json.zio.jsonBody
import sttp.tapir.server.ziohttp.ZioHttpInterpreter
import sttp.tapir.swagger.SwaggerUI
import sttp.tapir.ztapir._
import zio._
import zio.http.Server.defaultWithPort
import zio.redis._
import zio.schema.Schema
import zio.schema.codec.{BinaryCodec, ProtobufCodec}

object RestService extends ZIOAppDefault {

  private val redis = ZLayer.make[Redis & RedisExecutor & RedisConfig & CodecSupplier](
    Redis.layer,
    RedisExecutor.layer.orDie,
    ZLayer.succeed(RedisConfig.Default),
    ZLayer.succeed[CodecSupplier](new CodecSupplier {
      override def get[A: Schema]: BinaryCodec[A] = ProtobufCodec.protobufCodec
    })
  )

  private type RestPoint[In, Out] = PublicEndpoint[In, Unit, Out, Any]

  private val appStatusCheck: RestPoint[Unit, String] =
    endpoint.get.in("status").out(stringBody)
  private def appStatusLogic(): ZIO[Any, Nothing, String] =
    for {
      statusMsg <- ZIO.succeed("SERVICE IS UP")
    } yield statusMsg

  private val safePushEndpoint: RestPoint[(String, String, SnapCmds), Long] =
    endpoint.post
      .in("safe-push" / path[String] / path[String])
      .in(jsonBody[SnapCmds])
      .out(jsonBody[Long])
  private def safePushLogic(gameId: String, round: String, cmds: List[SnapCmd]): ZIO[Any, Nothing, Long] =
    RedisService
      .safePush(gameId, round, cmds)
      .provideLayer(redis)
      .orElseSucceed(-1)

  private val loadSnapsEndpoint: RestPoint[String, HexList] =
    endpoint.get.in("snap" / "load" / path[String]).out(jsonBody[HexList])

  private def loadSnapsLogic(gameId: String): ZIO[Any, Nothing, HexList] =
    RedisService
      .loadHexSnaps(gameId)
      .orElse(ZIO.succeed(HexList()))
      .provideLayer(redis)

  private val appRoutes = List(
    appStatusCheck.zServerLogic(_ => appStatusLogic()),
    safePushEndpoint.zServerLogic { case (gameId, round, cmds) =>
      safePushLogic(gameId, round, cmds.cmds)
    },
    loadSnapsEndpoint.zServerLogic { gameId => loadSnapsLogic(gameId) }
  )

  private val openApi = OpenAPIDocsInterpreter()
    .toOpenAPI(appRoutes.map(_.endpoint), "BeOne", "1.0")
  private val swaggerUI = SwaggerUI[Task](openApi.toYaml)

  private val swgHttp = ZioHttpInterpreter().toHttp(swaggerUI)
  private val appHttp = ZioHttpInterpreter().toHttp(appRoutes)
  private val http    = appHttp ++ swgHttp

  override def run: ZIO[ZIOAppArgs with Scope, Any, Any] = for {
    server <- zio.http.Server
      .serve(http)
      .provide(defaultWithPort(AppConfig.configuration.http.port))
      .exitCode
  } yield server

}

package service

import domain.Hex
import sttp.apispec.openapi.circe.yaml._
import sttp.tapir.PublicEndpoint
import sttp.tapir.docs.openapi._
import sttp.tapir.generic.auto._
import sttp.tapir.json.zio.jsonBody
import sttp.tapir.server.ziohttp.ZioHttpInterpreter
import sttp.tapir.swagger.SwaggerUI
import sttp.tapir.ztapir._
import zio.Config.int
import zio._
import zio.config.typesafe.TypesafeConfigProvider
import zio.redis.{CodecSupplier, Redis, RedisConfig, RedisExecutor}
import zio.schema.Schema
import zio.schema.codec.{BinaryCodec, ProtobufCodec}

object RestService extends ZIOAppDefault {

  override val bootstrap: ZLayer[Any, Nothing, Unit] =
    Runtime.setConfigProvider(TypesafeConfigProvider.fromResourcePath())

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
  private def appStatusLogic(): ZIO[Any, Nothing, String] = for {
    statusMsg <- ZIO.succeed("SERVICE IS UP")
  } yield statusMsg

  private val loadSnapsEndpoint: RestPoint[String, List[Hex]] =
    endpoint.get.in("load" / path[String]).out(jsonBody[List[Hex]])

  private def loadSnapsLogic(gameId: String): ZIO[Any, Nothing, List[Hex]] =
    RedisService
      .loadHexSnaps(gameId)
      .orElse(ZIO.succeed(Nil))
      .provideLayer(redis)

  private val saveSnapsEndpoint: RestPoint[(String, String, List[Hex]), Long] =
    endpoint.post
      .in("save" / path[String] / path[String])
      .in(jsonBody[List[Hex]])
      .out(jsonBody[Long])

  private def saveSnapsLogic(gameId: String, round: String, snaps: List[Hex]): ZIO[Any, Nothing, Long] =
    ZIO
      .succeed(scala.util.Try { round.toLong } getOrElse 0L)
      .flatMap { round =>
        RedisService
          .saveHexSnaps(gameId, round, snaps)
          .orElse(ZIO.succeed(0L))
      }
      .provideLayer(redis)

  private val appRoutes = List(
    appStatusCheck.zServerLogic(_ => appStatusLogic()),
    saveSnapsEndpoint.zServerLogic { case (gameId, round, snaps) => saveSnapsLogic(gameId, round, snaps) },
    loadSnapsEndpoint.zServerLogic { gameId => loadSnapsLogic(gameId) }
  )

  private val openApi = OpenAPIDocsInterpreter()
    .toOpenAPI(appRoutes.map(_.endpoint), "BeOne", "1.0")
  private val swaggerUI = SwaggerUI[Task](openApi.toYaml)

  private val swgHttp = ZioHttpInterpreter().toHttp(swaggerUI)
  private val appHttp = ZioHttpInterpreter().toHttp(appRoutes)
  private val http    = appHttp ++ swgHttp

  override def run: ZIO[ZIOAppArgs with Scope, Any, Any] = for {
    port <- ZIO.config(int("http-port"))
    server <- zio.http.Server
      .serve(http)
      .provide(zio.http.Server.defaultWithPort(port))
      .exitCode
  } yield server

}

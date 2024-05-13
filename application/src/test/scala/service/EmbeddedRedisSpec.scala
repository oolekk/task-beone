package service

import domain.GameSnap
import zio._
import zio.redis.Input._
import zio.redis.Output.LongOutput
import zio.redis._
import zio.redis.embedded.EmbeddedRedis
import zio.schema.Schema
import zio.schema.codec.{BinaryCodec, ProtobufCodec}
import zio.test._

object EmbeddedRedisSpec extends ZIOSpecDefault {

  implicit val stringIn = StringInput
  implicit val longOut  = LongOutput

  object ProtobufCodecSupplier extends CodecSupplier {
    def get[A: Schema]: BinaryCodec[A] = ProtobufCodec.protobufCodec
  }

  def snapgen(n: Int = 0): List[GameSnap] = (0 until n).map(_ => GameSnap.gen()).toList
  private val ALL: Range.Inclusive        = 0 to -1

  def spec: Spec[TestEnvironment with Scope, RedisError] =
    suite("RedisService")(
      test("pushSnapsGetNewCount appends to redis list when round equals the list size, returns new list size") {
        for {
          threeMore    <- RedisService.saveGameSnaps("test", 0, snapgen(3))
          fetch3       <- RedisService.pullListItems("test", ALL).map(_.length)
          twoMore      <- RedisService.saveGameSnaps("test", 3, snapgen(2))
          fetch5       <- RedisService.pullListItems("test", ALL).map(_.length)
          oneMore      <- RedisService.saveGameSnaps("test", 5, snapgen(1))
          offByOneLess <- RedisService.saveGameSnaps("test", 5, snapgen(1))
          offByOneMore <- RedisService.saveGameSnaps("test", 7, snapgen(1))
          offByTwoMore <- RedisService.saveGameSnaps("test", 8, Nil)
          fetch6       <- RedisService.pullListItems("test", ALL).map(_.length)
        } yield assertTrue(
          threeMore == 3,
          fetch3 == 3,
          twoMore == 5,
          fetch5 == 5,
          oneMore == 6,
          offByOneLess == 6,
          offByOneMore == 6,
          offByTwoMore == 6,
          fetch6 == 6
        )
      }
    ).provideShared(
      EmbeddedRedis.layer.orDie,
      RedisExecutor.layer.orDie,
      ZLayer.succeed[CodecSupplier](ProtobufCodecSupplier),
      Redis.layer
    ) @@ TestAspect.silentLogging
}

package service

import domain.GameSnap
import service.RedisService.{pullListItems, pushStringsGetNewCount}
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

  def saveGameSnaps(gameId: String, round: Long, items: List[GameSnap]): ZIO[Redis, RedisError, Long] =
    pushStringsGetNewCount(gameId, round, items.map(_.asHexStr))

  object ProtobufCodecSupplier extends CodecSupplier {
    def get[A: Schema]: BinaryCodec[A] = ProtobufCodec.protobufCodec
  }

  def snapgen(n: Int = 0): List[GameSnap] = (0 until n).map(_ => GameSnap.gen()).toList
  private val ALL: Range.Inclusive        = 0 to -1

  def spec: Spec[TestEnvironment with Scope, RedisError] =
    suite("RedisService")(
      test("pushSnapsGetNewCount appends to redis list when round equals the list size, returns new list size") {
        for {
          threeMore    <- saveGameSnaps("test", 0, snapgen(3))
          fetch3       <- pullListItems("test", ALL).map(_.length)
          twoMore      <- saveGameSnaps("test", 3, snapgen(2))
          fetch5       <- pullListItems("test", ALL).map(_.length)
          oneMore      <- saveGameSnaps("test", 5, snapgen(1))
          offByOneLess <- saveGameSnaps("test", 5, snapgen(1))
          offByOneMore <- saveGameSnaps("test", 7, snapgen(1))
          offByTwoMore <- saveGameSnaps("test", 8, Nil)
          fetch6       <- pullListItems("test", ALL).map(_.length)
        } yield assertTrue(
          threeMore == 3,
          fetch3 == 3,
          twoMore == 5,
          fetch5 == 5,
          oneMore == 6,
          offByOneLess == -6,
          offByOneMore == -6,
          offByTwoMore == -6,
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

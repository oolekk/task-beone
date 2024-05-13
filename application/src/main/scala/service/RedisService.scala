package service

import domain.{FailedToFetchSnaps, GameSnap, SnapError, Hex}
import zio.redis.Input.StringInput
import zio.redis.Output.LongOutput
import zio.redis.{Redis, RedisError}
import zio.{Chunk, ZIO}

object RedisService {

  implicit val stringIn = StringInput
  implicit val longOut  = LongOutput

  def loadHexSnaps(gameId: String): ZIO[Redis, RedisError, List[Hex]] =
    pullListItems(gameId, 0 to -1).map(_.map(Hex(_)))

  def saveHexSnaps(gameId: String, round: Long, items: List[Hex]): ZIO[Redis, RedisError, Long] =
    pushItemsGetNewCount[Hex](gameId, round, items)(_.hex)

  def loadGameSnaps(gameId: String): ZIO[Redis, SnapError, List[GameSnap]] =
    pullListItems(gameId, 0 to -1)
      .mapError(err => FailedToFetchSnaps(err.getMessage))
      .flatMap(hex => ZIO.foreach(hex)(GameSnap.zioFromHex))

  def saveGameSnaps(gameId: String, round: Long, items: List[GameSnap]): ZIO[Redis, RedisError, Long] =
    pushItemsGetNewCount[GameSnap](gameId, round, items)(_.asHexStr)

  def pullListItems(key: String, range: Range): ZIO[Redis, RedisError, List[String]] =
    for {
      redis <- ZIO.service[Redis]
      items <- redis.lRange(key, range).returning[String]
    } yield items.toList

  def pushItemsGetNewCount[A](key: String, round: Long, items: List[A])(f: A => String): ZIO[Redis, RedisError, Long] =
    for {
      redis <- ZIO.service[Redis]
      status <- redis
        .eval(
          script = luaPushItemsGetNewCount,
          keys = Chunk(key, s"$round"),
          args = Chunk.fromIterable(items.map(f))
        )
        .returning[Long]
    } yield status

  private val luaPushItemsGetNewCount: String =
    """
      |local key = KEYS[1]
      |local round = tonumber(KEYS[2])
      |local size = redis.call('LLEN', key)
      |if size == round then
      |  for i = 1, #ARGV do
      |    redis.call('RPUSH', key, ARGV[i])
      |  end
      |  return size + #ARGV
      |else
      |  return size
      |end
      |""".stripMargin.trim

}

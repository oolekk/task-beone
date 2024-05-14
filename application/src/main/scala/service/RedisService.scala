package service

import domain.{GameSnap, HexList}
import zio.redis.Input.StringInput
import zio.redis.Output.LongOutput
import zio.redis.{Redis, RedisError}
import zio.{Chunk, ZIO}

object RedisService {

  implicit val stringIn = StringInput
  implicit val longOut  = LongOutput

  def loadHexSnaps(gameId: String): ZIO[Redis, RedisError, HexList] =
    pullListItems(gameId, 0 to -1).map(HexList(_))

  def saveHexSnaps(gameId: String, round: Long, hexList: HexList): ZIO[Redis, RedisError, Long] =
    pushStringsGetNewCount(gameId, round, hexList.hxs)

  def saveGameSnaps(gameId: String, round: Long, items: List[GameSnap]): ZIO[Redis, RedisError, Long] =
    pushStringsGetNewCount(gameId, round, items.map(_.asHexStr))

  def pullListItems(key: String, range: Range): ZIO[Redis, RedisError, List[String]] =
    for {
      redis <- ZIO.service[Redis]
      items <- redis.lRange(key, range).returning[String]
    } yield items.toList

  def pushStringsGetNewCount(key: String, round: Long, items: List[String]): ZIO[Redis, RedisError, Long] =
    for {
      redis <- ZIO.service[Redis]
      status <- redis
        .eval(
          script = luaPushItemsGetNewCount,
          keys = Chunk(key, s"$round"),
          args = Chunk.fromIterable(items)
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

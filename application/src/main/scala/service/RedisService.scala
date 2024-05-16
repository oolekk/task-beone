package service

import domain.{GameSnap, HexList, SnapCmd}
import zio.redis.Input.StringInput
import zio.redis.Output.LongOutput
import zio.redis.{Input, Output, Redis, RedisError}
import zio.{Chunk, ZIO}

object RedisService {

  implicit val stringIn: Input[String] = StringInput
  implicit val longOut: Output[Long]   = LongOutput

  def pushIfValid(gameId: String, round: String, cmds: List[SnapCmd]): ZIO[Redis, RedisError, Long] = {
    // validation: last stored snap must match the next one with the move reverted
    collectArgs(round, cmds) collect { case (intRound, prevSnap) =>
      RedisService.validatedPush(gameId, intRound, prevSnap, cmds.map(_.snap))
    } getOrElse ZIO.succeed(-1)
  }
  private def collectArgs(round: String, cmds: List[SnapCmd]): Option[(Int, GameSnap)] = for {
    intRound                    <- round.toIntOption
    SnapCmd(firstCmd, nextSnap) <- cmds.headOption
    prevSnap                    <- nextSnap.revert(firstCmd).toOption
    changeList = (prevSnap :: cmds.map(_.snap)).sliding(2, 1).toList.zip(cmds.map(_.cmd))
    if changeList.forall { case (List(prev, next), cmd) => prev.update(cmd).contains(next) }
  } yield (intRound, prevSnap)

  def loadHexSnaps(gameId: String): ZIO[Redis, RedisError, HexList] =
    pullListItems(gameId, 0 to -1).map(HexList(_))

  def saveGameSnaps(gameId: String, round: Long, items: List[GameSnap]): ZIO[Redis, RedisError, Long] =
    pushStringsGetNewCount(gameId, round, items.map(_.asHexStr))

  def pullListItems(key: String, range: Range): ZIO[Redis, RedisError, List[String]] =
    for {
      redis <- ZIO.service[Redis]
      items <- redis.lRange(key, range).returning[String]
    } yield items.toList

  private def validatedPush(
    key: String,
    round: Long,
    prevSnap: GameSnap,
    snaps: List[GameSnap]
  ): ZIO[Redis, RedisError, Long] =
    for {
      redis <- ZIO.service[Redis]
      status <- redis
        .eval(
          script = luaPushIfValidGetNewCount,
          keys = Chunk(key, s"$round", s"$prevSnap"),
          args = Chunk.fromIterable(snaps.map(_.asHexStr))
        )
        .returning[Long]
    } yield status

  private val luaPushIfValidGetNewCount: String =
    """
      |local key = KEYS[1]
      |local round = tonumber(KEYS[2])
      |local size = redis.call('LLEN', key)
      |if size == round then
      |  if size == 0 or redis.call('LRANGE', key, size, size)[1] ~= KEYS[3] then
      |    for i = 1, #ARGV do
      |      redis.call('RPUSH', key, ARGV[i])
      |    end
      |    return size + #ARGV
      |  else
      |    return size
      |  end
      |else
      |  return size
      |end
      |""".stripMargin.trim

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

package service

import domain.GameSnap
import zio._
import zio.json._
import zio.kafka.consumer.{Consumer, ConsumerSettings, Subscription}
import zio.kafka.serde.Serde
import zio.stream.{ZSink, ZStream}

import java.io.File
import service.BrokerConf._

object SnapConsumer extends ZIOAppDefault {

  private val consumer: ZLayer[Any, Throwable, Consumer] =
    ZLayer.scoped(Consumer.make(ConsumerSettings(List(BROKER)).withGroupId(SNAP_GROUP)))

  val snapSerde: Serde[Any, GameSnap] = Serde.string.inmapM(string =>
    ZIO
      .fromEither(string.fromJson[GameSnap])
      .mapError(msg => new RuntimeException(msg))
  )(gameSnap => ZIO.from(gameSnap.toJson))

  private val stringStream = Consumer
    .plainStream(Subscription.topics(SNAP_TOPIC), Serde.string, snapSerde)
    .map(record => record.value.toJson)
    .intersperse("\n")
    .flatMap(json => ZStream.fromIterable(json.getBytes))

  def runStream(gameId: String) = stringStream.run(
    ZSink.fromFile(new File(s"./$gameId"))
  ).provide(consumer)

  def run = runStream("stuff").exit

}

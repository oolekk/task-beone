package service

import SnapSerde._
import zio._
import zio.config.typesafe.TypesafeConfigProvider
import zio.json._
import zio.kafka.consumer.{Consumer, ConsumerSettings, Subscription}
import zio.kafka.serde.Serde
import zio.stream.ZStream

object SnapConsumer extends ZIOAppDefault {

  override val bootstrap: ZLayer[Any, Nothing, Unit] =
    Runtime.setConfigProvider(TypesafeConfigProvider.fromResourcePath())

  override def run = {
    val stream = for {
      broker    <- ZIO.config(Config.string("broker-host-port"))
      snapTopic <- ZIO.config(Config.string("broker-snap-topic"))
      snapGroup <- ZIO.config(Config.string("broker-snap-group"))
      consumer = Consumer.make(ConsumerSettings(List(broker)).withGroupId(snapGroup))
      stringStream = Consumer
        .plainStream(Subscription.topics(snapTopic), Serde.string, snapSerde)
        .map(record => record.value.toJson)
        .intersperse("\n")
        .flatMap(json => ZStream.fromIterable(json.getBytes))
    } yield stringStream
    stream.exit
  }

}

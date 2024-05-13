package service

import SnapSerde.snapSerde
import domain.GameSnap
import org.apache.kafka.clients.producer.ProducerRecord
import zio._
import zio.config.typesafe.TypesafeConfigProvider
import zio.kafka.producer._
import zio.kafka.serde._
import zio.stream.ZStream

object SnapProducer extends ZIOAppDefault {

  override val bootstrap: ZLayer[Any, Nothing, Unit] =
    Runtime.setConfigProvider(TypesafeConfigProvider.fromResourcePath())

  override def run: ZIO[ZIOAppArgs with Scope, Any, Unit] = for {
    broker    <- ZIO.config(Config.string("broker-host-port"))
    snapTopic <- ZIO.config(Config.string("broker-snap-topic"))
    streamWriter = ZLayer.scoped(Producer.make(ProducerSettings(List(broker))))
    stream <- ZStream
      .repeatZIO(Clock.currentDateTime)
      .schedule(Schedule.spaced(3.second))
      .map(time => new ProducerRecord(snapTopic, time.getMinute.toString, GameSnap.empty))
      .via(Producer.produceAll(Serde.string, snapSerde))
      .drain
      .runDrain
      .provide(streamWriter)
  } yield stream

}

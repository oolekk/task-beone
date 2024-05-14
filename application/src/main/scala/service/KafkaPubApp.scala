package service

import SnapSerde.snapSerde
import configuration.AppConfig
import domain.GameSnap
import org.apache.kafka.clients.producer.ProducerRecord
import zio._
import zio.kafka.producer._
import zio.kafka.serde._
import zio.stream.ZStream
import AppConfig.configuration

object KafkaPubApp extends ZIOAppDefault {

  override def run: ZIO[ZIOAppArgs with Scope, Any, Unit] = {

    val broker       = configuration.kafka.address
    val topic        = configuration.kafka.topic
    val streamWriter = ZLayer.scoped(Producer.make(ProducerSettings(List(broker))))

    for {
      stream <- ZStream
        .repeatZIO(Clock.currentDateTime)
        .schedule(Schedule.spaced(3.second))
        .map(time => new ProducerRecord(topic, time.getMinute.toString, GameSnap.empty))
        .via(Producer.produceAll(Serde.string, snapSerde))
        .drain
        .runDrain
        .provide(streamWriter)
    } yield stream
  }

}

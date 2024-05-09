package service

import domain.GameSnap
import service.BrokerConf.BROKER
import zio.{ZIO, ZLayer}
import zio.kafka.producer.{Producer, ProducerSettings}
import zio.kafka.serde.Serde
import zio.json._

object SnapProducer {

  private val snapSerde: Serde[Any, GameSnap] =
    Serde.string.inmapM(string =>
      ZIO
        .fromEither(string.fromJson[GameSnap])
        .mapError(msg => new RuntimeException(msg))
    )(nextSnap => ZIO.from(nextSnap.toJson))

  val snapProducer: Unit =
    ZLayer.scoped {
      Producer.produceAll(Serde.string, snapSerde)
      Producer.make(ProducerSettings(List(BROKER)))
    }

}

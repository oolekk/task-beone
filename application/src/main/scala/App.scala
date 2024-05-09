import domain.GameSnap
import org.apache.kafka.clients.producer.ProducerRecord
import service.BrokerConf.SNAP_TOPIC
import util.GameSnapUtil
import zio.ConfigProvider.console
import zio.kafka.consumer.{CommittableRecord, Consumer, ConsumerSettings, Subscription}
import zio.{Chunk, Clock, Schedule, Scope, Task, ZIO, ZIOAppArgs, ZIOAppDefault, ZLayer, durationInt}
import zio.kafka.producer.{Producer, ProducerSettings}
import zio.kafka.serde._
import zio.stream.{ZSink, ZStream}
import zio.json._
import zio.Console._

import java.io.File

object App extends ZIOAppDefault {

  private val producerSettings = ProducerSettings(List("localhost:9092"))
  private val streamWriter     = ZLayer.scoped(Producer.make(producerSettings))

  case class Player(name: String, score: Int)

  object Player {
    implicit val decoder: JsonDecoder[Player] = DeriveJsonDecoder.gen[Player]
    implicit val encoder: JsonEncoder[Player] = DeriveJsonEncoder.gen[Player]
  }

  val snapSerde: Serde[Any, GameSnap] = Serde.string.inmapM(string =>
    ZIO
      .fromEither(string.fromJson[GameSnap])
      .mapError(msg => new RuntimeException(msg))
  )(boardSnap => ZIO.from(boardSnap.toJson))

  val fileWritingLayer =
    ZLayer.succeed(ZSink.fromFile(new File("./outputFile.txt")))


  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = {

    val producerStream: ZStream[Producer, Throwable, Nothing] =
      ZStream
        .repeatZIO(Clock.currentDateTime)
        .schedule(Schedule.spaced(3.second))
        .map(time => new ProducerRecord(SNAP_TOPIC, time.getMinute.toString, GameSnap.empty))
        .via(Producer.produceAll(Serde.string, snapSerde))
        .drain
    producerStream.runDrain.provide(streamWriter)
  }

}

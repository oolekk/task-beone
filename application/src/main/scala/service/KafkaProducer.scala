package service

import configuration.AppConfig.configuration
import domain.{BoardCmd, SnapCmd}
import org.apache.kafka.clients.producer.ProducerRecord
import zio._
import zio.kafka.producer._
import zio.kafka.serde._
import zio.stream.ZStream

object KafkaProducer {

  private val broker = configuration.kafka.address
  private val topic  = configuration.kafka.topic

  private lazy val streamWriter = ZLayer.scoped(Producer.make(ProducerSettings(List(broker))))

  def produceBoardCmdRecs(gameId: String, snapCmds: List[SnapCmd]): ZIO[Any, Throwable, Unit] = {
    ZStream
      .fromIterable(snapCmds.map(snapCmd => new ProducerRecord(topic, gameId, snapCmd.cmd)))
      .via(Producer.produceAll(Serde.string, BoardCmd.serde))
      .runDrain
      .provide(streamWriter)
  }

}

package app

import configuration.ClientConfig.configuration
import domain.GameSnap.asXY
import domain._
import zio.Console.printLine
import zio.kafka.consumer._
import zio.kafka.serde.Serde
import zio._

object ConsumerApp extends ZIOAppDefault {

  private val broker = configuration.kafka.address
  private val topic  = configuration.kafka.topic
  private val group  = configuration.kafka.group

  private val consumerLayer: ZLayer[Any, Throwable, Consumer] =
    ZLayer.scoped(Consumer.make(ConsumerSettings(List(broker)).withGroupId(group)))

  private def consumerData(topic: String) =
    Consumer
      .plainStream(Subscription.topics(topic), Serde.string, BoardCmd.serde)
      .map(record => record -> record.value)

  override def run: ZIO[Any, Throwable, ExitCode] =
    for {
      _ <- consumerData(topic)
        .provideLayer(consumerLayer)
        .foreach { case (rec, value) =>
          printLine(s"GameId:${rec.key} " + (value match {
            case RookAdded(at)         => s"Rook added onto ${asXY(at)}"
            case RookTaken(at)         => s"Rook taken from ${asXY(at)}"
            case RookMoved(from, to)   => s"Rook moved from ${asXY(from)} to ${asXY(to)}"
            case BishopAdded(at)       => s"Bishop added onto ${asXY(at)}"
            case BishopTaken(at)       => s"Bishop taken from ${asXY(at)}"
            case BishopMoved(from, to) => s"Bishop moved from ${asXY(from)} to ${asXY(to)}"
          }))
        }
        .forever
    } yield ExitCode.success

}

package app

package service

import configuration.ClientConfig.configuration
import zio.kafka.consumer.{Consumer, ConsumerSettings, Subscription}
import zio.kafka.serde.Serde

object SnapConsumer {

  def run = {
    val broker   = configuration.kafka.address
    val topic    = configuration.kafka.topic
    val group    = configuration.kafka.group
    val consumer = Consumer.make(ConsumerSettings(List(broker)).withGroupId(group))
    for {
      _ <- Consumer
        .plainStream(Subscription.topics(topic), Serde.string, Serde.string)
        .intersperse("\n")
    } yield ()
  }

}

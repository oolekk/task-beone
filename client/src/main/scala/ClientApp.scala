import configuration.AppConfiguration
import zio.kafka.consumer.{Consumer, ConsumerSettings, Subscription}
import zio.kafka.serde.Serde
import zio.{Scope, UIO, ZIO, ZIOAppArgs, ZIOAppDefault, ZLayer}

/*
  This is only test code, made to make testing easier. Please do not use it in production or use it as an example.
 */
object ClientApp extends ZIOAppDefault with AppConfiguration {
  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = consumerStream

  private val consumerSettings: ConsumerSettings =
    ConsumerSettings(List(configuration.kafka.address))
      .withGroupId(configuration.kafka.group)
      .withClientId(configuration.kafka.client)

  private val subscription = Subscription.topics(configuration.kafka.topic)

  private val consumerLayer = ZLayer.scoped(Consumer.make(consumerSettings))

  private val consumerStream: UIO[Unit] = Consumer
    .plainStream(
      subscription,
      Serde.string,
      Serde.string
    )
    .tap { record => ZIO.log(s"Received ${record.key}: ${record.value}") }
    .mapZIO(_.offset.commit)
    .provideLayer(consumerLayer)
    .runDrain
    .orDie

}

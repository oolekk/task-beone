package configuration

import com.typesafe.config.Config
import pureconfig.ConfigObjectSource
import pureconfig.ConfigSource
import pureconfig.generic.auto._

case class ClientConfig(
  kafka: KafkaConfiguration,
  rest: RestConfiguration
)
case class KafkaConfiguration(
  address: String,
  topic: String,
  group: String,
  client: String
)
case class RestConfiguration(
  statusUrl: String,
  loadGameUrl: String,
  saveGameUrl: String
)

object ClientConfig extends Configuration {
  def apply(config: Config): ClientConfig = {
    val configSource: ConfigObjectSource = ConfigSource.fromConfig(config.getConfig("client"))
    configSource.loadOrThrow[ClientConfig]
  }
  val configuration: ClientConfig = ClientConfig(config)
}

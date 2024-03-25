package configuration

import com.typesafe.config.Config
import pureconfig.ConfigObjectSource
import pureconfig.ConfigSource
import pureconfig.generic.auto._

case class Configuration(
  kafka: KafkaConfiguration
)
case class KafkaConfiguration(
  address: String,
  topic: String,
  group: String,
  client: String
)

object Configuration {
  def apply(config: Config): Configuration = {
    val configSource: ConfigObjectSource = ConfigSource.fromConfig(config.getConfig("client"))
    configSource.loadOrThrow[Configuration]
  }
}

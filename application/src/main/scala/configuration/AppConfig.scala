package configuration

import com.typesafe.config.Config
import pureconfig.ConfigObjectSource
import pureconfig.ConfigSource
import pureconfig.generic.auto._

case class AppConfig(
  kafka: KafkaConfiguration,
  http: HttpConfiguration
)
case class KafkaConfiguration(
  address: String,
  topic: String,
  group: String,
  client: String
)
case class HttpConfiguration (
  host: String,
  port: Int
)

object AppConfig extends Configuration {
  def apply(config: Config): AppConfig = {
    val configSource: ConfigObjectSource = ConfigSource.fromConfig(config.getConfig("app"))
    configSource.loadOrThrow[AppConfig]
  }
  val configuration: AppConfig = AppConfig(config)
}

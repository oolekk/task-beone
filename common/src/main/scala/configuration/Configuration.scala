package configuration

import com.typesafe.config.{Config, ConfigFactory}

trait Configuration {
  ConfigFactory.invalidateCaches()

  val config: Config = ConfigFactory
    .systemEnvironment()
    .withFallback(ConfigFactory.systemProperties())
    .withFallback(ConfigFactory.defaultApplication())
    .resolve()

}

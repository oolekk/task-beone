import sbt.*
import sbt.librarymanagement.ModuleID

object Dependencies {

  private val config = Seq(
    "com.github.pureconfig" %% "pureconfig" % "0.17.6",
    "com.typesafe"           % "config"     % "1.4.3"
  )

  private val testing = Seq(
    "dev.zio" %% "zio-redis-embedded" % "0.2.0" % Test,
    "dev.zio" %% "zio-test"           % "2.1.0" % Test,
    "dev.zio" %% "zio-test-magnolia"  % "2.1.1" % Test,
    "dev.zio" %% "zio-test-sbt"       % "2.1.1" % Test
  )

  private val zio = Seq(
    "dev.zio" %% "zio"         % "2.1.0",
    "dev.zio" %% "zio-http"    % "3.0.0-RC6",
    "dev.zio" %% "zio-json"    % "0.6.2",
    "dev.zio" %% "zio-kafka"   % "2.7.4",
    "dev.zio" %% "zio-prelude" % "1.0.0-RC24"
  )

  private val service = Seq(
    "dev.zio" %% "zio-redis"           % "0.2.0",
    "dev.zio" %% "zio-schema-protobuf" % "1.0.1",
    "dev.zio" %% "zio-streams"         % "2.1.1"
  )

  private val tapir = Seq(
    "com.softwaremill.sttp.tapir" %% "tapir-json-zio"          % "1.10.6",
    "com.softwaremill.sttp.tapir" %% "tapir-openapi-docs"      % "1.10.7",
    "com.softwaremill.sttp.tapir" %% "tapir-swagger-ui"        % "1.10.6",
    "com.softwaremill.sttp.tapir" %% "tapir-swagger-ui-bundle" % "1.10.7",
    "com.softwaremill.sttp.tapir" %% "tapir-zio"               % "1.10.6",
    "com.softwaremill.sttp.tapir" %% "tapir-zio-http-server"   % "1.10.7"
  )

  val common: Seq[ModuleID]      = zio ++ config ++ testing
  val application: Seq[ModuleID] = common ++ service ++ tapir
  val client: Seq[ModuleID]      = common

}

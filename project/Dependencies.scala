import sbt._
import sbt.librarymanagement.ModuleID

object Dependencies {

  private val config = Seq(
    "com.typesafe"           % "config"     % "1.4.3",
    "com.github.pureconfig" %% "pureconfig" % "0.17.6"
  )

  private val scalatest = Seq(
    "org.scalatest" %% "scalatest"         % "3.2.18" % Test,
    "dev.zio"       %% "zio-test"          % "2.0.22" % Test,
    "dev.zio"       %% "zio-test-sbt"      % "2.0.22" % Test,
    "dev.zio"       %% "zio-test-magnolia" % "2.0.22" % Test
  )

  private val tapir = Seq(
    "com.softwaremill.sttp.tapir" %% "tapir-zio"               % "1.10.6",
    "com.softwaremill.sttp.tapir" %% "tapir-zio-http-server"   % "1.10.6",
    "com.softwaremill.sttp.tapir" %% "tapir-json-zio"          % "1.10.6",
    "com.softwaremill.sttp.tapir" %% "tapir-openapi-docs"      % "1.10.6",
    "com.softwaremill.sttp.tapir" %% "tapir-swagger-ui"        % "1.10.6",
    "com.softwaremill.sttp.tapir" %% "tapir-swagger-ui-bundle" % "1.10.6"
  )

  private val zio = Seq(
    "dev.zio" %% "zio"         % "2.0.22",
    "dev.zio" %% "zio-nio"     % "2.0.2",
    "dev.zio" %% "zio-prelude" % "1.0.0-RC24",
    "dev.zio" %% "zio-openai"  % "0.4.1",
    "dev.zio" %% "zio-http"    % "3.0.0-RC6",
    "dev.zio" %% "zio-streams" % "2.0.22",
    "dev.zio" %% "zio-kafka"   % "2.7.4",
    "dev.zio" %% "zio-json"    % "0.6.2"
  )

  val common: Seq[ModuleID]      = zio ++ scalatest
  val application: Seq[ModuleID] = zio ++ tapir ++ scalatest
  val client: Seq[ModuleID]      = config ++ zio ++ scalatest

}

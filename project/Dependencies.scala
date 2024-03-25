import sbt._
import sbt.librarymanagement.ModuleID

object Dependencies {

  private val config = Seq(
    "com.typesafe"           % "config"     % "1.4.3",
    "com.github.pureconfig" %% "pureconfig" % "0.17.6"
  )

  lazy val zio: Seq[ModuleID] = Seq(
    "dev.zio" %% "zio"       % "2.0.21",
    "dev.zio" %% "zio-kafka" % "2.7.4"
  )

  lazy val tapir: Seq[ModuleID] = Seq("com.softwaremill.sttp.tapir" %% "tapir-zio-http-server" % "1.10.0")

  val application: Seq[ModuleID] = zio ++ tapir
  val client: Seq[ModuleID]      = config ++ zio
}

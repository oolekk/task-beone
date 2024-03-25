import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

object App extends ZIOAppDefault {
  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ZIO.log("hello world")
}

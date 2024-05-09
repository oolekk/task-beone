package com.vowsum.ziodot

import zio.Console.ConsoleLive
import zio.http._
import zio.{Scope, ZIOAppDefault}

/*
  This is only test code, made to make testing easier. Please do not use it in production or use it as an example.
 */
object RestClientApp extends ZIOAppDefault {
  def run = for {
    resp <- Client
      .request(Request.get("http://localhost:7090/hello"))
      .provide(Client.default, Scope.default)
    body <- resp.body.asString
    _    <- ConsoleLive.printLine(body)
  } yield ()

}

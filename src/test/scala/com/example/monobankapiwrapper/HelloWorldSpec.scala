package com.example.monobankapiwrapper

import cats.effect.IO
import org.http4s._
import org.http4s.implicits._
import munit.CatsEffectSuite
import org.http4s


class HelloWorldSpec extends CatsEffectSuite {

  test("HelloWorld returns status code 200") {
    assertIO(retHelloWorld.map(_.status), Status.Ok)
    assertIO(retHelloWorld.flatMap(_.as[String]), "{\"message\":\"Hello, world\"}")
  }


  val server: http4s.HttpApp[IO] = {
    val helloWorld = HelloWorld.impl[IO]
    MonobankapiwrapperRoutes.helloWorldRoutes(helloWorld).orNotFound
  }

  private[this] val retHelloWorld: IO[Response[IO]] = {
    val getHW = Request[IO](Method.GET, uri"/hello/world")
    val helloWorld = HelloWorld.impl[IO]
    MonobankapiwrapperRoutes.helloWorldRoutes(helloWorld).orNotFound(getHW)
  }
}
package com.example.monobankapiwrapper

import cats.effect.{ExitCode, IO, IOApp}

object Main extends IOApp {
  def run(args: List[String]) =
    MonobankapiwrapperServer.stream[IO].compile.drain.as(ExitCode.Success)
}

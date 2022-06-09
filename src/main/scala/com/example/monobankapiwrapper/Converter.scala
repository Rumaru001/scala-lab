package com.example.monobankapiwrapper

import cats.Applicative
import cats.implicits._
import io.circe.{Encoder, Json}
import org.http4s.EntityEncoder
import org.http4s.circe._

trait Converter[F[_]]{
  def db(n: Converter.Value): F[Converter.Data]
  def bd(n: Converter.Value): F[Converter.Data]
}


object Converter {
  implicit def apply[F[_]](implicit ev: Converter[F]): Converter[F] = ev

  final case class Value(value: String) extends AnyVal
  final case class Data(data: String) extends AnyVal

  object Data {
    implicit val greetingEncoder: Encoder[Data] = new Encoder[Data] {
      final def apply(a: Data): Json = Json.obj(
        ("converted", Json.fromString(a.data)),
      )
    }
    implicit def greetingEntityEncoder[F[_]]: EntityEncoder[F, Data] =
      jsonEncoderOf[F, Data]
  }

  def impl[F[_]: Applicative]: Converter[F] = new Converter[F]{
    def db(n: Converter.Value): F[Converter.Data] =
      Data("" + NumberConverter.decimalToBinary(n.value)).pure[F]
    def bd(n: Converter.Value): F[Converter.Data] =
      Data("" + NumberConverter.binaryToDecimal(n.value)).pure[F]
  }
}
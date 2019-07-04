package ch.becompany.csv

import java.time.format.DateTimeFormatter
import java.time.{Duration, LocalDate, LocalTime}

import cats.implicits._

import scala.reflect.ClassTag

trait Parser[A] {
  def apply(s: String): Either[String, A]
}

object Parser {

  def catchNonFatal[A : ClassTag](parse: String => A): Parser[A] =
    s => Either.catchNonFatal(parse(s))
      .leftMap("Failed to parse " + implicitly[ClassTag[A]].runtimeClass.getSimpleName + ": " + _.getMessage)

  implicit val string: Parser[String] =
    (s: String) => Right(s)

  implicit val date: Parser[LocalDate] =
    catchNonFatal(LocalDate.parse(_, DateTimeFormatter.ISO_DATE))

  implicit val time: Parser[LocalTime] =
    catchNonFatal(LocalTime.parse(_, DateTimeFormatter.ISO_TIME))

  implicit val duration: Parser[Duration] =
    time(_).map(d => Duration.ofSeconds(d.getHour * 3600 + d.getMinute * 60 + d.getSecond))

}

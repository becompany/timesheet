package ch.becompany.timesheet

import java.io.{File, PrintWriter}
import java.nio.file.Paths
import java.time.{Duration, LocalDate, LocalTime}
import java.util.concurrent.Executors

import cats.{Eq, Monoid}
import cats.effect.{Blocker, ExitCode, IO, IOApp, Resource}
import cats.implicits._
import ch.becompany.csv.{LineParser, Parser}
import enumeratum.values.{StringEnum, StringEnumEntry}
import fs2.{Pipe, Stream}

import scala.concurrent.ExecutionContext
import scala.io.{BufferedSource, Source}

object Main extends IOApp {

  lazy val outputFileName = "target/timesheet.csv"

  final case class Line(user: String,
                        email: String,
                        client: String,
                        project: Project,
                        task: String,
                        description: String,
                        billable: String,
                        startDate: LocalDate,
                        startTime: LocalTime,
                        endDate: LocalDate,
                        endTime: LocalTime,
                        duration: Duration)

  lazy val lineParser: LineParser[Line] = LineParser[Line]

  implicit lazy val dateEq: Eq[LocalDate] = Eq.instance(_ isEqual _)

  implicit lazy val durationMonoid: Monoid[Duration] =
    new Monoid[Duration] {
      override def empty: Duration = Duration.ZERO

      override def combine(x: Duration, y: Duration): Duration = x plus y
    }

  sealed abstract class Project(val value: String, val number: String) extends StringEnumEntry

  object Project extends StringEnum[Project] {

    case object Produktentwicklungen extends Project("Produktentwicklungen", "8-20160-1007")
    case object Aoe extends Project("Archivöffnung", "4-20192-0001")
    case object Betrieb extends Project("Betrieb", "8-20163-0010")
    case object VideoWorkflow extends Project("Video Workflow", "8-20160-1008")
    case object Tms extends Project("TMS", "8-20160-1009")

    override def values: IndexedSeq[Project] = findValues

    implicit lazy val parser: Parser[Project] =
      s => Project.withValueOpt(s).toRight("Unknown project '" + s + "'")

    implicit lazy val eq: Eq[Project] =
      Eq.fromUniversalEquals

    implicit lazy val ordering: Ordering[Project] =
      Ordering.by(_.number)
  }

  def durationToDouble(duration: Duration): Double =
    (duration.toMinutes.toDouble / 60 * 4).ceil / 4

  def convert(lines: List[String]): Either[List[String], String] =
    lines
      .filter(l => l.trim.nonEmpty && !l.contains("User,Email,Client"))
      .map(_.split(',').toList)
      .traverse(lineParser.apply)
      .map { lineList =>
        (
          lineList
            .groupBy(e => (e.startDate, e.project))
            .toList
            .sortBy(_._1)
            .map { case ((date, project), chunk) =>
              val t = chunk.foldMap(e => durationToDouble(e.duration))
              List(
                date,
                project.number,
                project.value,
                "%.2f".format(t),
                150,
                "%.2f".format(t * 150)
              )
            } ++: List(List("")) ++: lineList
            .groupBy(_.project)
            .toList
            .sortBy(_._1)
            .map { case (project, chunk) =>
              val t = chunk.foldMap(e => durationToDouble(e.duration))
              List(
                project.number,
                project.value,
                "%.2f".format(t * 150)
              )
            } ++: List(List(""))
          )
          .map(_.mkString("\t"))
          .mkString("\n")
      }

  def fileResource(name: String): Resource[IO, BufferedSource] =
    Resource.fromAutoCloseable(IO.delay(Source.fromFile(name)))

  def writerResource(name: String): Resource[IO, PrintWriter] =
    Resource.fromAutoCloseable(IO.delay(new PrintWriter(name)))


  def run(args: List[String]): IO[ExitCode] =
    args match {
      case fileName :: Nil =>
        Tuple2
          .apply(
            fileResource(fileName),
            writerResource(outputFileName)
          )
          .tupled
          .use { case (in, out) =>
            for {
              lines <- IO.delay(in.getLines().toList)
              output <- IO.fromEither(convert(lines).leftMap(e => new Exception(e.mkString("; "))))
              _ <- IO.delay(out.write(output))
            } yield ExitCode.Success
          }

      case _ =>
        IO.delay(println("Usage: run <filename>")).as(ExitCode.Error)
    }

}

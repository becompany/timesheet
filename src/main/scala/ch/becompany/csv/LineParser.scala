package ch.becompany.csv

import shapeless.{::, Generic, HList, HNil}

trait LineParser[Out] {
  def apply(l: List[String]): Either[List[String], Out]
}

object LineParser {

  implicit val hnilParser: LineParser[HNil] = new LineParser[HNil] {
    def apply(s: List[String]): Either[List[String], HNil] =
      s match {
        case Nil => Right(HNil)
        case h +: t => Left(List(s"""Expected end, got "$h"."""))
      }
  }

  implicit def hconsParser[H: Parser, T <: HList : LineParser]: LineParser[H :: T] = {
    case Nil => Left(List("Excepted list element."))
    case h +: t =>
      val head = implicitly[Parser[H]].apply(h)
      val tail = implicitly[LineParser[T]].apply(t)
      (head, tail) match {
        case (Left(error), Left(errors)) => Left(error :: errors)
        case (Left(error), Right(_)) => Left(error :: Nil)
        case (Right(_), Left(errors)) => Left(errors)
        case (Right(h), Right(t)) => Right(h :: t)
      }
  }

  implicit def caseClassParser[Out, R <: HList](
                                                 implicit gen: Generic[Out] {type Repr = R},
                                                 reprParser: LineParser[R]): LineParser[Out] =
    (s: List[String]) => reprParser.apply(s).right.map(gen.from)

  def apply[Out](implicit parser: LineParser[Out]): LineParser[Out] =
    parser
}
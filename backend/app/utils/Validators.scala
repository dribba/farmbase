package utils

import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat

import scalaz._
import Scalaz._
import scala.util.Try
import scala.util.Success
import scala.util.Failure

object Validators {

  trait StringToEnum[+A] {
    def valueOf(str: String): Option[A]
  }

  def nonEmptyString(str: String): ValidationNel[String, String] =
    if (str.isEmpty) "Cannot be empty.".failureNel[String]
    else str.successNel[String]


  def minLength(min: Int)(str: String): ValidationNel[String, String] =
    if (str.length < min) s"Must be at least $min characters long but got ${str.length}.".failureNel[String]
    else str.successNel[String]

  def maxLength(max: Int)(str: String): ValidationNel[String, String] =
    if (str.length > max) s"Must be at most $max characters long but got ${str.length}.".failureNel[String]
    else str.successNel[String]


  def lengthBetween(min: Int, max: Int)(str: String): ValidationNel[String, String] =
    (minLength(min)(str) |@| maxLength(max)(str)).tupled.map(_._1) // FIXME

  def min(min: Int)(value: Int): ValidationNel[String, Int] =
    if (value < min) s"Must be at least $min but got $value.".failureNel[Int]
    else value.successNel[String]

  def max(max: Int)(value: Int): ValidationNel[String, Int] =
    if (value > max) s"Must be at most $max but got $value.".failureNel[Int]
    else value.successNel[String]


  def between(minValue: Int, maxValue: Int)(value: Int): ValidationNel[String, Int] =
    (min(minValue)(value) |@| max(maxValue)(value)).tupled.map(_._1) // FIXME

  def oneOf[A](values: Iterable[String])(value: String)(implicit fromStr: StringToEnum[A]): ValidationNel[String, A] =
    fromStr.valueOf(value) match {
      case Some(a) => a.successNel[String]
      case None => s"Must be one of ${values.toString} but got $value.".failureNel[A]
    }

  def dateTime(value: String): ValidationNel[String, DateTime] =
    Try(DateTime.parse(value, ISODateTimeFormat.hourMinuteSecond())) match {
      case Success(a) => a.successNel[String]
      case Failure(_) => "Invalid date format".failureNel[DateTime]
    }

  def dateTime(value: Long): ValidationNel[String, DateTime] =
    Try(new DateTime(value)) match {
      case Success(a) => a.successNel[String]
      case Failure(_) => "Invalid date".failureNel[DateTime]
    }

}

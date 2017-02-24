package models

import org.joda.time.DateTime
import models.CropMedia.CropMedia
import models.FeedingType.FeedingType
import play.api.libs.json._
import play.api.libs.functional.syntax._
import utils.Validators._

import scalaz.ValidationNel
import scalaz.Scalaz._

object CropMedia extends Enumeration {
  type CropMedia = Value

  val Hydroponics = Value("Hydroponics")
  val Soil = Value("Soil")
  val Coco = Value("Coco")
  val Aeroponics = Value("Aeroponics")

  implicit object StringToCropMedia extends StringToEnum[CropMedia] {
    def valueOf(str: String): Option[CropMedia] = CropMedia.valueOf(str)
  }

  def valueOf(str: String): Option[CropMedia] =
    values.find(_.toString == str)
}
object FeedingType extends Enumeration {
  type FeedingType = Value

  val Compost = Value("Compost")
  val Fertilizer = Value("Fertilizer")
  val Nutrients = Value("Nutrients")

  implicit object StringToCropMedia extends StringToEnum[FeedingType] {
    def valueOf(str: String): Option[FeedingType] = FeedingType.valueOf(str)
  }

  def valueOf(str: String): Option[FeedingType] =
    values.find(_.toString == str)
}

case class Crop
  ( _type: String
  , variety: String
  , quantity: Int
  , media: CropMedia
  , feeding: FeedingType
  , id: Option[String]
  , started: Option[DateTime]
  )

object Crop {

  private def validType(cropType: String): ValidationNel[String, String] =
    nonEmptyString(cropType)

  private def validVariety(cropVariety: String): ValidationNel[String, String] =
    nonEmptyString(cropVariety)

  private def validQuantity(cropQuantity: Int): ValidationNel[String, Int] =
    between(1, 500)(cropQuantity)

  private def validMedia(cropMedia: String): ValidationNel[String, CropMedia] =
    oneOf[CropMedia](CropMedia.values.map(_.toString))(cropMedia)

  private def validFeeding(feeding: String): ValidationNel[String, FeedingType] =
    oneOf[FeedingType](FeedingType.values.map(_.toString))(feeding)

  private def validStarted(date: Long): ValidationNel[String, DateTime] =
    dateTime(date)

  private def validStartedOpt(date: Option[Long]): ValidationNel[String, Option[DateTime]] =
    date.map(validStarted).map(_.map(Some(_))).getOrElse(None.successNel[String])


  def fromJson(jsValue: JsValue): ValidationNel[String, Crop] =
    rd.reads(jsValue) match {
      case JsSuccess(crop, _) => crop
      case JsError(e) => ("Error parsing json" + e).failureNel[Crop]
    }

  implicit val rd =
    ( (__ \ "type").read[String].map(validType) and
      (__ \ "variety").read[String].map(validVariety) and
      (__ \ "quantity").read[Int].map(validQuantity) and
      (__ \ "media").read[String].map(validMedia) and
      (__ \ "feeding").read[String].map(validFeeding) and
      (__ \ "id").readNullable[String].map(_.successNel[String]) and
      (__ \ "started").readNullable[Long].map(validStartedOpt)
    ) (_ |@| _ |@| _ |@| _ |@| _ |@| _ |@| _)
      .map(_.apply(Crop.apply))


  implicit val wr = Json.writes[Crop]

//  def create(raw: RawCrop): ValidationNel[String, Crop] =
//      ( validType(raw._type) |@|
//        validVariety(raw.variety) |@|
//        validQuantity(raw.quantity) |@|
//        validMedia(raw.media) |@|
//        validFeeding(raw.feeding) |@|
//        raw.id.successNel |@|
//        raw.started.map(validStartedOpt).getOrElse(None.successNel[String])
//      ).apply(Crop.apply)

}

package utils.json

import play.api.libs.json.{JsArray, Json, Writes}

import scalaz.NonEmptyList


object Writers {

  implicit def nel[A](implicit wr: Writes[A]): Writes[NonEmptyList[A]] = Writes(nel => {
    JsArray(nel.list.toList.map(Json.toJson(_)))
  })
}

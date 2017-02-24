package utils.bson

import org.joda.time.DateTime
import org.mongodb.scala.bson.{BsonDateTime, BsonString, BsonTransformer, BsonValue}

object Transformers {

  implicit val date = new BsonTransformer[DateTime] {
    override def apply(value: DateTime): BsonValue =
      BsonDateTime(value.toDate)
  }

  implicit val enum = new BsonTransformer[Enumeration#Value] {
    override def apply(value: Enumeration#Value): BsonValue =
      BsonString(value.toString)
  }

}

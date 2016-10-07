package utils.json

import java.io.{StringWriter, Writer}

import org.bson.BsonDocument
import org.bson.codecs.{BsonDocumentCodec, EncoderContext}
import org.bson.json.{JsonWriter, JsonWriterSettings}
import org.bson.types.ObjectId
import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat
import org.mongodb.scala.bson.Document

class FlatJsonWriter(writer: Writer, settings: JsonWriterSettings) extends JsonWriter(writer, settings) {

  override def doWriteDateTime(time: Long): Unit = {
    val dt = new DateTime(time)
    writeString(dt.toString(ISODateTimeFormat.dateHourMinuteSecond()))
  }

  override def doWriteObjectId(objectId: ObjectId): Unit = {
    writeString(objectId.toString)
  }
}

object Bson2Json {

  def toJson(document: BsonDocument) = {
    val settings = new JsonWriterSettings()
    val writer: StringWriter = new StringWriter
    new BsonDocumentCodec().encode(new FlatJsonWriter(writer, settings), document, EncoderContext.builder.build)
    writer.toString
  }


}
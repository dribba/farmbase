package controllers

import org.bson.types.ObjectId
import org.joda.time.DateTime
import org.mongodb.scala._
import org.mongodb.scala.model.Filters
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.mvc.{Action, Controller}
import utils.json.Bson2Json

import scala.concurrent.{Future, Promise}

class Crops extends Controller {

  val mongoClient: MongoClient = MongoClient()
  val database: MongoDatabase = mongoClient.getDatabase("farmbase")
  val collection = database.getCollection("crops")

  private def asFutureSingle[A](obs: Observable[A]): Future[A] = {
    val prom = Promise[A]
    obs.subscribe(new Observer[A] {
      override def onNext(result: A): Unit = prom.success(result)

      override def onError(e: Throwable): Unit = prom.failure(e)

      override def onComplete(): Unit = prom.failure(new Exception("No results"))
    })
    prom.future
  }

  private def toJson(doc: Document): JsValue =
    Json.parse(Bson2Json.toJson(doc.toBsonDocument))

  def get = Action.async { request =>
    collection.find().limit(20).toFuture().map(docs => {
      // So ugly, but for now works
      Ok(JsArray(docs.map(toJson)))
    })
  }

  def remove(id: String) = Action.async { request =>
    collection.deleteOne(Document("_id" -> new ObjectId(id))).toFuture().map(result => {
      val deleted: Long = result.headOption.fold(0L)(_.getDeletedCount)
      if (deleted > 0) Ok(Json.obj("status" -> "ok"))
      else NotFound(Json.obj("status" -> "error"))
    })
  }

  private val cropReads = (
    (__ \ "name").read[String] and
    (__ \ "variety").read[String] and
    (__ \ "planted").read[DateTime]
  ).tupled

  private def createCrop(name: String, variety: String, planted: DateTime): Document =
    Document(
      "_id" -> ObjectId.get(),
      "name" -> name,
      "variety" -> variety,
      "planted" -> planted.toDate
    )

  def add = Action.async(parse.json(2 * 1024)) { request =>
    cropReads.reads(request.body) match {
      case JsSuccess((name, variety, planted), _) =>
        val doc = createCrop(name, variety, planted)
        collection.insertOne(doc).toFuture().map(_ => {
          Created(toJson(doc))
        })

      case JsError(_) =>
        Future.successful(BadRequest("Invalid request"))
    }
  }

}
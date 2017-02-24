package controllers

import org.bson.types.ObjectId
import org.joda.time.DateTime
import org.mongodb.scala._
import org.mongodb.scala.model.Filters
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.functional.syntax._
import utils.bson.Transformers._
import utils.json.Writers._
import play.api.libs.json._
import play.api.mvc.{Action, Controller}
import utils.json.Bson2Json
import models.Crop

import scala.concurrent.{Future, Promise}
import scalaz.{Failure, Success}


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

  private def toDocument(crop: Crop): Document =
    Document(
      "type" -> crop._type,
      "variety" -> crop.variety,
      "quantity" -> crop.quantity,
      "media" -> crop.media,
      "feeding" -> crop.feeding,
      "_id" -> crop.id,
      "started" -> crop.started
    )

  def add = Action.async(parse.json(2 * 1024)) { request =>
    Crop.fromJson(request.body) match {
      case Success(crop) =>
        val newCrop = crop.copy(id = Some(ObjectId.get().toString))
        collection.insertOne(toDocument(newCrop)).toFuture().map(_ => {
          Created(Json.toJson(newCrop))
        })

      case Failure(errors) =>
        Future.successful(BadRequest(Json.toJson(errors)))
    }
  }

  def edit(id: String) = Action.async(parse.json(2 * 1024)) { request =>
    Crop.fromJson(request.body) match {
      case Success(crop) =>
        val docId = crop.id.getOrElse(ObjectId.get().toString)
        val safeCrop = crop.copy(id = Some(docId))
        def withId = Document("_id" -> docId)

        collection.replaceOne(withId, toDocument(safeCrop)).toFuture().map(_ => {
          Accepted(Json.toJson(safeCrop))
        })

      case Failure(errors) =>
        Future.successful(BadRequest(Json.toJson(errors)))
    }
  }

}

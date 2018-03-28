package edu.knoldus.entities

import play.api.libs.json.{Format, Json}

case class ExternalData(userId: Int, id: Int, title: String, body: String) {}

object ExternalData {
  implicit val format: Format[ExternalData]= Json.format
}
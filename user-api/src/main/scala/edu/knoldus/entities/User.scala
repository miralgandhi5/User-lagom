package edu.knoldus.entities

import play.api.libs.json.{Format, Json}

case class User(id: Int,name: String, age: Int) {


}

object User {
  implicit val format: Format[User] = Json.format
}

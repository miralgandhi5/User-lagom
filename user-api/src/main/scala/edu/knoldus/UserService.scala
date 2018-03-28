package edu.knoldus

import akka.{Done, NotUsed}
import com.lightbend.lagom.scaladsl.api.transport.Method._
import com.lightbend.lagom.scaladsl.api.{Descriptor, Service, ServiceCall}
import edu.knoldus.entities.{ExternalData, User, UserData}

trait UserService extends Service {

  override final def descriptor: Descriptor = {
    import Service._
    // @formatter:off
    named("user-lagom")
      .withCalls(
        pathCall("/api/user/:id", hello _),
        restCall(POST, "/api/user/addUser", addUser()),
        restCall(PUT, "/api/user/updateUser/:id", updateUser _),
        restCall(DELETE, "/api/user/deleteUser/:id", deleteUser _),
        pathCall("/api/user/getUser/:id", getUser _),
        pathCall("/api/user/external", getExternal _)

      )


  }.withAutoAcl(true)

  def hello(id: String): ServiceCall[NotUsed, String]

  def addUser(): ServiceCall[User, Done]

  def getUser(id: Int): ServiceCall[NotUsed, User]

  def deleteUser(id: Int): ServiceCall[NotUsed, String]

  def updateUser(id: Int): ServiceCall[UserData, String]


  def getExternal(): ServiceCall[NotUsed, ExternalData]
}

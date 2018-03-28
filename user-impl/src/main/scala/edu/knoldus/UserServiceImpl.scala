package edu.knoldus

import akka.{Done, NotUsed}
import com.lightbend.lagom.scaladsl.api.ServiceCall
import edu.knoldus.entities.{ExternalData, User, UserData}
import play.api.Logger

import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future}

class UserServiceImpl(externalService: ExternalService)(implicit ec: ExecutionContext) extends UserService {
  val userList: ListBuffer[User] = ListBuffer.empty[User]

  /**
    * says hello to given id.
    *
    * @param id name to greet.
    * @return string is returned.
    */
  override def hello(id: String) = ServiceCall { _ =>
    Future.successful(s"hello $id")
  }

  /**
    * adds User to userList.
    *
    * @return done.
    */
  override def addUser(): ServiceCall[User, Done] = ServiceCall { request =>
    val user = User(request.id, request.name, request.age)
    userList += user
    Future.successful(Done)
  }

  /**
    * gets user of given id.
    *
    * @param id id of user.
    * @return user with given id if found else dummy user.
    */
  override def getUser(id: Int): ServiceCall[NotUsed, User] = ServiceCall { _ =>
    val userOptional = userList.find(user => user.id == id)
    userOptional.fold(Future.successful(User(0, "not found", 0)))(user => Future.successful(user))
  }

  /**
    * deletes user of given id.
    *
    * @param id id of user.
    * @return list of existing user.
    */
  override def deleteUser(id: Int): ServiceCall[NotUsed, String] = ServiceCall { _ =>
    val user = userList.filter(user => user.id == id)
    userList --= user
    Logger.info(s"$userList")
    Future.successful(s"$userList")
  }

  /**
    * updates user of given id.
    *
    * @param id id of user.
    * @return all users with update.
    */
  override def updateUser(id: Int): ServiceCall[UserData, String] = ServiceCall { updateUser =>
    val userOptional: Option[User] = userList.find(user => user.id == id)
    userOptional match {
      case Some(user) => userList -= user
        val newUser: User = user.copy(name = updateUser.name, age = updateUser.age)
        userList += newUser
        Future.successful(s" in some $userList")
      case None => Future.successful(s"in none $userList")
    }
  }

  /**
    * calls external unManaged service.
    *
    * @return json returned by external service.
    */
  def getExternal(): ServiceCall[NotUsed, ExternalData] = ServiceCall { _ =>
    externalService.testService().invoke()
  }
}

package edu.knoldus

import com.lightbend.lagom.scaladsl.api.ServiceLocator
import com.lightbend.lagom.scaladsl.api.ServiceLocator.NoServiceLocator
import com.lightbend.lagom.scaladsl.devmode.LagomDevModeComponents
import com.lightbend.lagom.scaladsl.server.{LagomApplication, LagomApplicationContext, LagomApplicationLoader, LagomServer}
import com.softwaremill.macwire.wire
import play.api.libs.ws.ahc.AhcWSComponents

class UserLagomLoader extends LagomApplicationLoader {
  override def load(context: LagomApplicationContext): LagomApplication =
    new UserLagomApplication(context) {
      override def serviceLocator: ServiceLocator = NoServiceLocator
    }

  override def loadDevMode(context: LagomApplicationContext): LagomApplication =
    new UserLagomApplication(context) with LagomDevModeComponents {

    }

  override def describeService = Some(readDescriptor[UserService])
}

abstract class UserLagomApplication(context: LagomApplicationContext)
  extends LagomApplication(context) with AhcWSComponents {

  // Bind the service that this server provides

  override lazy val lagomServer: LagomServer = serverFor[UserService](wire[UserServiceImpl])
  lazy val externalService: ExternalService = serviceClient.implement[ExternalService]

}




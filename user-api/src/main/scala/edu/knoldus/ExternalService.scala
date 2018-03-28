package edu.knoldus

import akka.NotUsed
import com.lightbend.lagom.scaladsl.api.{Descriptor, Service, ServiceCall}
import edu.knoldus.entities.ExternalData

trait ExternalService extends Service {

  override final def descriptor: Descriptor = {
    import Service._
    // @formatter:off
    named("external-service")
      .withCalls(
        pathCall("/posts/1", testService _)
      )


  }.withAutoAcl(true)

  def testService(): ServiceCall[NotUsed, ExternalData]

}

package org.no.ip.bca.superlearn

import org.no.ip.bca.scala.Ranges
import se.scalablesolutions.akka.actor.{Actor, ActorRef}

class Bridge(parent: ActorRef) extends Actor {
  private var clients = Set.empty[ActorRef]
  private var config: Option[ClientConfig] = None
  private var ranges: Option[Ranges] = None
  private var state: Option[State] = None

  private var compute: Option[Compute] = None
  private var computeCount = 0

  override def init = {
    parent ! ServerActor.Connect
    log.info("Started bridge")
  }

  def receive = {
    case msg@ServerActor.Request(_) =>
      parent forward msg

    case msg@ClientActor.Assigned(_, range) =>
      sendToAllClients(msg)
      ranges = ranges map { _ / range }

    case ServerActor.RecieveCompute(compute) =>
      this.compute = this.compute map { _ + compute } orElse Some(compute)
      computeCount += 1
      if (computeCount == clients.size) {
        sendCompute
      }

    case msg@ClientActor.NewConfig(config) =>
      sendToAllClients(msg)
      this.config = Some(config)

    case msg@ClientActor.NewState(state) =>
      sendToAllClients(msg)
      this.state = Some(state)
    case msg@ClientActor.NewRanges(ranges) =>
      sendToAllClients(msg)
      this.ranges = Some(Ranges(ranges))

    case ServerActor.Connect =>
      self.sender foreach { sender =>
        clients += sender
        config foreach { sender ! ClientActor.NewConfig(_) }
        state foreach { sender ! ClientActor.NewState(_) }
        ranges foreach { r => sender ! ClientActor.NewRanges(r.parts) }
      }
  }

  private def sendToAllClients(msg: AnyRef) = clients foreach { _ ! msg }
  private def sendCompute = {
    parent ! ServerActor.RecieveCompute(compute.get)
    compute = None
    computeCount = 0
  }
}

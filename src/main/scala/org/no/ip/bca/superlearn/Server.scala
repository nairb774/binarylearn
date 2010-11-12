package org.no.ip.bca.superlearn

import math._

import org.no.ip.bca.scala.Ranges
import se.scalablesolutions.akka.actor.{Actor, ActorRef}
import se.scalablesolutions.akka.remote.RemoteNode

object Server {
  def main(args: Array[String]): Unit = {
    ConfigHelper.bootstrap
    val server = Actor.actorOf(new ServerActor(ConfigHelper.fullRange, ConfigHelper.matrixRecorder)).start
    RemoteNode.start(ConfigHelper.hostname, ConfigHelper.serverPort)
    RemoteNode.register("bridge", Actor.actorOf(new Bridge(server)).start)
  }
}

final case class ServerState(state: State, momentum: State, compute: Compute, configState: ConfigState) {
  def +(newCompute: Compute): ServerState = {
    val newConfig = ConfigHelper.configState

    val count = newCompute.count.toDouble
    val cdNorm = newCompute.cd / count
    val hActNorm = newCompute.hAct / count
    val vActNorm = newCompute.vAct / count

    val newWeightMomentum = momentum.weights * newConfig.momentumMix + cdNorm * (1.0 - newConfig.momentumMix)
    val newHiddenBiasMomentum = momentum.hidden * newConfig.momentumMix + hActNorm * (1.0 - newConfig.momentumMix)
    val newVisibleBiasMomentum = momentum.visible * newConfig.momentumMix + vActNorm * (1.0 - newConfig.momentumMix)
    val newMomentum = State(newWeightMomentum, newHiddenBiasMomentum, newVisibleBiasMomentum)

    val newWeights = state.weights + newWeightMomentum * newConfig.epsilon
    val newHiddenBias = state.hidden + newHiddenBiasMomentum * newConfig.epsilon
    val newVisibleBias = state.visible + newVisibleBiasMomentum * newConfig.epsilon

    val newState = State(newWeights, newHiddenBias, newVisibleBias)
    ServerState(newState, newMomentum, newCompute, newConfig)
  }
}

object ServerActor {
  final case class RecieveCompute(compute: Compute)
  final case class Request(range: Ranges.Pair)
  final case object Connect
}

class ServerActor(
  fullRange: Ranges,
  matrixRecorder: ActorRef) extends Actor {
  import ServerActor._
  private var client: Option[ActorRef] = None
  private var serverState: ServerState = null
  private var clientConfig: ClientConfig = null
  private var outstanding = fullRange
  private var compute: Compute = null

  // Stats
  private var requests: Long = 0
  private var requestSuccess: Long = 0
  private var timeStart = System.nanoTime

  override def init: Unit = {
    serverState = matrixRecorder.!!(MatrixRecorder.Latest, ConfigHelper.timeout).getOrElse(throw new RuntimeException("Could not get latest state")).asInstanceOf[ServerState]
    sendToClient
  }

  def receive = {
    case RecieveCompute(compute) => shift(compute)
    case Request(range) =>
      requests += 1
      if ((outstanding & range) == Ranges(range)) {
        requestSuccess += 1
        val id = self.sender.get.uuid
        client foreach { _ ! ClientActor.Assigned(id, range) }
        outstanding /= range
      } else if (outstanding.isEmpty) {
        // Do nothing
      } else {
        val newRange = outstanding.head.withMaxLength(range.end - range.start)
        self forward Request(newRange)
      }
    case Connect =>
      client = self.sender
      client foreach { c =>
        c ! ClientActor.NewConfig(ConfigHelper.clientConfig)
        c ! ClientActor.NewState(serverState.state)
        c ! ClientActor.NewRanges(outstanding.parts)
      }
  }

  private def shift(compute: Compute) = {
    println("LOCK: " + requestSuccess.toDouble / requests.toDouble + " " + requests + " " + requestSuccess + " " + (requests - requestSuccess))
    serverState += compute
    matrixRecorder ! MatrixRecorder.Record(System.currentTimeMillis, serverState)
    sendToClient
  }

  private def sendToClient = {
    outstanding = fullRange
    val newState = serverState.state
    val parts = outstanding.parts
    val sendClientConfig = {
      val newClientConfig = ConfigHelper.clientConfig
      val r = newClientConfig != clientConfig
      clientConfig = newClientConfig
      r
    }
    val time = System.nanoTime - timeStart
    timeStart = System.nanoTime
    if (sendClientConfig) {
      println(clientConfig)
      client foreach { _ ! ClientActor.NewConfig(clientConfig) }
    }
    client foreach { c =>
      c ! ClientActor.NewState(newState)
      c ! ClientActor.NewRanges(parts)
    }
  }
}

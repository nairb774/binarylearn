package org.no.ip.bca.superlearn

import java.util.UUID

import net.lag.configgy.Configgy

import org.no.ip.bca.scala.Ranges
import org.no.ip.bca.scala.utils.actor.ReActor
import math.{ Matrix, Vector }
import math.Implicits._
import math.Types._

object Server {
  import java.io._
  import java.net._

  def main(args: Array[String]): Unit = {
    Configgy configure args(0)
    val config = Configgy.config
    config.registerWithJmx("org.no.ip.bca.superlearn")
    
    Bridge2 { clientOutbound =>
      val server = new ServerActor(ConfigHelper.fullRange, ConfigHelper.matrixRecorder, ConfigHelper.state, clientOutbound)
      server.start
      new ServerActorBridge(server)
    } listen (ConfigHelper.serverPort, ConfigHelper.timeout)
  }
}

final case class ServerState(state: State, compute: Compute, configState: ConfigState) {
  def +(compute: Compute): ServerState = {
    val newConfig = ConfigHelper.configState
    val newWeights = applyContrastiveDivergence(newConfig.epsilon, state.weights, compute.cd, compute.count)
    val newVisibleBias = applyBiasCD(newConfig.epsilon, state.visible, compute.vAct, compute.count)
    val newHiddenBias = applyBiasCD(newConfig.epsilon, state.hidden, compute.hAct, compute.count)
    val newState = State(newWeights, newHiddenBias, newVisibleBias)
    ServerState(newState, compute, newConfig)
  }

  private def applyContrastiveDivergence[L, R](epsilon: Double, weights: MatrixD[L, R], cd: MatrixD[L, R], count: Long) = {
    val cdNorm = cd <*> (epsilon / count.toDouble)
    val newWeights = weights <+> cdNorm
    newWeights
  }

  private def applyBiasCD[S](epsilon: Double, bias: VectorD[S], cd: VectorD[S], count: Long) = {
    val cdNorm = cd <*> (epsilon / count.toDouble)
    val newBias = bias <+> cdNorm
    newBias
  }
}

private object ServerActor {
  case class RecieveCompute(compute: Compute)
  case class Request(id: UUID, range: Ranges.Pair)
}

trait ClientOutbound {
  def newConfig(clientConfig: ClientConfig): Unit
  def newWork(state: State, ranges: List[Ranges.Pair]): Unit
  def assigned(id: UUID, range: Ranges.Pair): Unit
}

class ServerActorBridge(server: ServerActor) extends ServerOutbound {
  import ServerActor._

  def request(id: UUID, range: Ranges.Pair) = server ! Request(id, range)
  def sendCompute(compute: Compute) = server ! RecieveCompute(compute)
}

class ServerActor(
  fullRange: Ranges,
  matrixRecorder: MatrixRecorder,
  private var serverState: ServerState,
  client: ClientOutbound) extends ReActor {
  import ServerActor._
  private type PF = PartialFunction[Any, Unit]
  private var clientConfig: ClientConfig = null
  private var outstanding = fullRange
  private var compute: Compute = null
  private val slope = 0.99

  // Stats
  private var requests: Long = 0
  private var requestSuccess: Long = 0
  private var timeStart = System.nanoTime

  override def init: Unit = {
    sendToClient
  }
  
  val reAct: PF = {
    case RecieveCompute(compute) => shift(compute)
    case Request(id, range) =>
      requests += 1
      val a = if ((outstanding & range) == Ranges(range)) {
        requestSuccess += 1
        range
      } else if (outstanding.isEmpty) {
        Ranges.Pair(0, 0)
      } else {
        outstanding.head.withMaxLength(range.end - range.start)
      }
      client.assigned(id, a)
      outstanding /= a
  }

  private def shift(compute: Compute) = {
    println("LOCK: " + requestSuccess.toDouble / requests.toDouble + " " + requests + " " + requestSuccess + " " + (requests - requestSuccess))
    serverState += compute
    matrixRecorder.record(serverState)
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
      client.newConfig(clientConfig)
    }
    client.newWork(newState, parts)
  }
}

package org.no.ip.bca.superlearn

import java.util.UUID

import org.no.ip.bca.scala.Ranges
import org.no.ip.bca.scala.utils.actor.ReActor

object Server {
  import java.io._
  import java.net._

  def main(args: Array[String]): Unit = {
    val props = new Props(args(0))
    Bridge2 { clientOutbound =>
      val server = new ServerActor(props.fullRange, props.matrixRecorder, props.state, clientOutbound)
      server.start
      new ServerActorBridge(server)
    } listen (props.serverPort, props.timeout)
  }
}

case class Momentum(momentum: Array[Double], hidden: Array[Double], visible: Array[Double])
case class Score(w: Double, h: Double, v: Double) {
  def total = w + h + v
}

final case class ServerState(state: State, momentum: Momentum, score: Score) {
  def +(compute: Compute): ServerState = {
    val config = Config.config.state
    val (wScore, newWeights, newMomentum) = applyContrastiveDivergence(config, config.wMomentum, momentum.momentum, state.weights, compute.cd, compute.count)
    val (vScore, newVisibleBias, newVisibleMomentum) = applyBiasCD(config, config.vMomentum, momentum.visible, state.visible, compute.vAct, compute.count)
    val (hScore, newHiddenBias, newHiddenMomentum) = applyBiasCD(config, config.hMomentum, momentum.hidden, state.hidden, compute.hAct, compute.count)
    val newState = State(newWeights, newHiddenBias, newVisibleBias)
    val m = Momentum(newMomentum, newHiddenMomentum, newVisibleMomentum)
    val score = Score(wScore, hScore, vScore)
    ServerState(newState, m, score)
  }

  private def applyContrastiveDivergence(config: ConfigState, momentumFactor: Double, momentums: Array[Double], weights: Array[Double], cd: Array[Long], count: Long) = {
    val epsilon = config.epsilon
    val weightcost = config.weightcost

    val newWeights = new Array[Double](weights.length)
    val newMomentums = new Array[Double](momentums.length)

    var i = 0
    var score = 0.0
    val cdLength = cd.length
    while (i < cdLength) {
      val cdNorm = cd(i).toDouble / count
      score += cdNorm * cdNorm
      newMomentums(i) = momentumFactor * momentums(i) + epsilon * (cdNorm - weightcost * weights(i))
      newWeights(i) = weights(i) + newMomentums(i)
      i += 1
    }
    (score, newWeights, newMomentums)
  }

  private def applyBiasCD(config: ConfigState, momentumFactor: Double, momentum: Array[Double], bias: Array[Double], cd: Array[Long], count: Long) = {
    val epsilon = config.epsilon
    val weightcost = config.weightcost

    val newMomentum = new Array[Double](momentum.length)
    val newBias = new Array[Double](bias.length)

    var i = 0
    var score = 0.0
    val cdLength = cd.length
    while (i < cdLength) {
      val cdNorm = cd(i).toDouble / count
      score += cdNorm * cdNorm
      newMomentum(i) = momentumFactor * momentum(i) + epsilon * cdNorm
      newBias(i) = bias(i) + newMomentum(i)
      i += 1
    }
    (score, newBias, newMomentum)
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
  private var clientConfig = Config.config.clientConfig
  private var outstanding = fullRange
  private var compute: Compute = null
  private val slope = 0.99

  // Stats
  private var requests: Long = 0
  private var requestSuccess: Long = 0
  private var timeStart = System.nanoTime

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
    val config = Config.config
    if (serverState.score.total < 0.1) {
      config.setSteps(config.getSteps + 1)
    }
    outstanding = fullRange
    val newState = serverState.state
    val parts = outstanding.parts
    val sendClientConfig = {
      val newClientConfig = config.clientConfig
      val r = newClientConfig != clientConfig
      clientConfig = newClientConfig
      r
    }
    val time = System.nanoTime - timeStart
    println("RATE: " + time + " " + (compute.count.toDouble / time * 1E9) + " " + time / compute.count)
    timeStart = System.nanoTime
    if (sendClientConfig) {
      client.newConfig(clientConfig)
    }
    client.newWork(newState, parts)
  }
}

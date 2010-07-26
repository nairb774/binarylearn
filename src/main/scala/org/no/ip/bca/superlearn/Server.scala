package org.no.ip.bca.superlearn

import java.util.UUID

import org.no.ip.bca.scala.Ranges
import org.no.ip.bca.scala.utils.actor.ReActor

object Server {
    import java.io._
    import java.net._
    
    def main(args: Array[String]): Unit = {
        val props = new Props(args(0))
        val port = props.serverPort
        val ssocket = new ServerSocket(port)
        val server = new ServerActor(props.dataManager, props.matrixRecorder, props.state)
        server.start
        handleSocket(props, server, ssocket)
    }
    
    private def handleSocket(props: Props, server: ServerActor, ssocket: ServerSocket): Unit = {
        val socket = ssocket.accept
        socket.setSoTimeout(props.timeout)
        val out = new InvokeOut(new DataOutputStream(new BufferedOutputStream(socket.getOutputStream))) {
            override def handleException(e: Exception) = {
                super.handleException(e)
                exit()
            }
        }
        out.start
        val bridge = new ServerActorBridge(server, out.open[ClientOutbound](0))
        val in = new InvokeIn(new DataInputStream(new BufferedInputStream(socket.getInputStream))) {
            override def run = try {
                super.run
            } finally {
                exit()
            }
        }
        in + (0 -> bridge)
        new Thread(in).start
        handleSocket(props, server, ssocket)
    }
}

case class Momentum(momentum: Array[Double], vis: Array[Double], hidden: Array[Double])

final case class ServerState(state: State, momentum: Momentum) {
    def +(compute: Compute): ServerState = {
        val config = Config.config.getState
        val (wScore, newWeights, newMomentum) = applyContrastiveDivergence(config, config.wMomentum, momentum.momentum, state.weights, compute.cd, compute.count)
        val (vScore, newVisibleBias, newVisMomentum) = applyContrastiveDivergence(config, config.vMomentum, momentum.vis, state.visible, compute.vAct, compute.count)
        val (hScore, newHiddenBias, newHidMomentum) = applyContrastiveDivergence(config, config.hMomentum, momentum.hidden, state.hidden, compute.hAct, compute.count)
        val newState = State(newWeights, newHiddenBias, newVisibleBias, config.sample, config.steps)
        println(List(wScore, vScore, hScore, wScore + vScore + hScore) mkString "\t")
        ServerState(newState, Momentum(newMomentum, newVisMomentum, newHidMomentum))
    }
    
    def applyContrastiveDivergence(config: ConfigState, momentumFactor: Double, momentums: Array[Double], weights: Array[Double], cd: Array[Long], count: Long) = {
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
}

private object ServerActor {
  case class Connect(client: ClientOutbound)
  case class RequestPoint(point: Long, client: ClientOutbound)
  case class RecieveCompute(compute: Compute, client: ClientOutbound)
  case class Request(id: UUID, range: Ranges.Pair, client: ClientOutbound)
}

trait ClientOutbound {
  def newWork(state: State, ranges: Ranges.Pair*): Unit
  def sendData(point: Long, data: Array[Byte]): Unit
  def assigned(id: UUID, range: Ranges.Pair): Unit
}

class ServerActorBridge(server: ServerActor, private var _client: ClientOutbound) extends ServerOutbound {
  import ServerActor._
  server ! Connect(_client)
  
  def request(id: UUID, range: Ranges.Pair) = server ! Request(id, range, _client)
  def sendCompute(compute: Compute) = server ! RecieveCompute(compute, _client)
  def requestPoint(point: Long) = server ! RequestPoint(point, _client)
}

class ServerActor(
    dataManager: DataManager,
    matrixRecorder: MatrixRecorder,
    private var serverState: ServerState
    ) extends ReActor {
  import ServerActor._
  private class ClientState {
    var done = false
    var assigned = Ranges.empty
  }
  private type PF = PartialFunction[Any, Unit]
  private var clients = Map.empty[ClientOutbound, ClientState]
  
  private var outstanding = dataManager.ranges
  private var compute: Compute = null
  private val slope = 0.99
  
  val reAct: PF = {
    case Connect(client) =>
      clients += client -> new ClientState()
      client.newWork(serverState.state, outstanding.parts: _*)
    case RequestPoint(point, client) =>
      client.sendData(point, dataManager.read(point))
    case RecieveCompute(compute, client) =>
      // Mark off client
      clients(client).done = true
      // Merge matrix
      if (compute != null) {
        if (this.compute == null) {
          this.compute = compute
        } else {
          this.compute += compute
        }
      }
      // Check for next round
      if (outstanding.isEmpty && clients.values.find(!_.done).isEmpty) {
        shift
      }
    case Request(id, range, client) =>
      if ((outstanding & range) == Ranges(range)) {
        clients.keys foreach { _.assigned(id, range) }
        outstanding /= range
        clients(client).assigned |= range
      }
  }
  
  private def shift = {
    serverState += compute
    matrixRecorder.record(serverState, 0.0)
    compute = null
    outstanding = dataManager.ranges
    val newState = serverState.state
    val parts = outstanding.parts
    clients foreach { case (client, state) =>
      client.newWork(newState, parts: _*)
      state.done = false
      state.assigned = Ranges.empty
    }
  }
}

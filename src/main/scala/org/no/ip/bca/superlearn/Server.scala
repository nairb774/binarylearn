package org.no.ip.bca.superlearn

import java.util.UUID

import org.no.ip.bca.scala.Ranges
import org.no.ip.bca.scala.utils.actor.ReActor

object Server {
    import java.io._
    import java.net._
    
    def main(args: Array[String]) = {
        val props = new Props(args(0))
        val port = props.serverPort
        val ssocket = new ServerSocket(port)
        val server = new ServerActor(props.dataManager, props.matrix)
        server.start
        handleSocket(server, ssocket)
    }
    
    def handleSocket(server: ServerActor, ssocket: ServerSocket): Unit = {
        val socket = ssocket.accept
        socket.setSoTimeout(10 * 1000)
        val out = new InvokeOut(new ObjectOutputStream(new BufferedOutputStream(socket.getOutputStream)))
        out.start
        val bridge = new ServerActorBridge(server, out.open(0))
        val in = new InvokeIn(new ObjectInputStream(new BufferedInputStream(socket.getInputStream)))
        in + (0 -> bridge)
        new Thread(in).start
        handleSocket(server, ssocket)
    }
}

private object ServerActor {
  case class Connect(client: ClientOutbound)
  case class Disconnect(client: ClientOutbound)
  case class RequestPoint(point: Long, client: ClientOutbound)
  case class RecieveMatrix(matrix: Matrix, count: Long, client: ClientOutbound)
  case class Request(id: UUID, range: Ranges.Pair, client: ClientOutbound)
}

trait ClientOutbound {
  def newWork(matrix: Matrix, ranges: Ranges.Pair*)
  def sendData(point: Long, data: Array[Byte]): Unit
  def assigned(id: UUID, range: Ranges.Pair): Unit
  def newRanges(ranges: Ranges.Pair*)
}

class ServerActorBridge(server: ServerActor, private var _client: ClientOutbound) extends ServerOutbound {
  import ServerActor._
  if (_client != null) {
      server ! Connect(_client)
  }
  
  /*def client_=(client: ClientOutbound) = {
    _client = client
    server ! Connect(client)
  }*/
  def request(id: UUID, range: Ranges.Pair) = server ! Request(id, range, _client)
  def sendMatrix(matrix: Matrix, count: Long) = server ! RecieveMatrix(matrix, count, _client)
  def requestPoint(point: Long) = server ! RequestPoint(point, _client)
}

class ServerActor(
    dataManager: DataManager,
    private var currentMatrix: Matrix
    ) extends ReActor {
  import ServerActor._
  private class ClientState {
    var done = false
    var assigned = Ranges.empty
  }
  private type PF = PartialFunction[Any, Unit]
  private var clients = Map.empty[ClientOutbound, ClientState]
  
  private var outstanding = dataManager.ranges
  private var inProgress: Matrix = null
  private var count: Long = 0
  private val slope = 0.99
  
  val reAct: PF = {
    case Connect(client) =>
      clients += client -> new ClientState()
      client.newWork(currentMatrix, outstanding.parts: _*)
    case Disconnect(client) =>
      val state = clients(client)
      val assigned = state.assigned.parts
      clients -= client
      if (outstanding.isEmpty) {
        // TODO: AHHH!
      } else {
        clients.keys foreach { _.newRanges(assigned: _*) }
      }
    case RequestPoint(point, client) =>
      client.sendData(point, dataManager.read(point))
    case RecieveMatrix(matrix, _count, client) =>
      // NOTE: matrix could be sent right before disconnect (not necessarily at end of round)
      // Mark off client
      clients(client).done = true
      // Merge matrix
      if (_count > 0) {
        if (inProgress == null) {
          inProgress = matrix
        } else {
          UtilMethods.sum(matrix.m, inProgress.m)
        }
        // Store count
        count += _count
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
    println(System.nanoTime)
    UtilMethods.mult(inProgress.m, slope / count.toDouble)
    currentMatrix = inProgress
    inProgress = null
    count = 0
    outstanding = dataManager.ranges
    val parts = outstanding.parts
    clients foreach { case (client, state) =>
      client.newWork(currentMatrix, outstanding.parts: _*)
      state.done = false
      state.assigned = Ranges.empty
    }
  }
}

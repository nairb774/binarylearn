package org.no.ip.bca.superlearn

import java.io._
import java.net._

import java.util.UUID

import net.lag.configgy.Configgy
import net.lag.logging.Logger

import org.no.ip.bca.scala.{ ActiveProxy, InvokeIn, InvokeOut, LoggingSupport, Ranges }
import org.no.ip.bca.scala.utils.actor.ReActor

object Bridge2 {
  trait ToClient extends ClientOutbound {
    def connect(client: ClientOutbound): Unit
  }
  private def getToClient: ToClient = ActiveProxy[ToClient](new ToClientImpl().asInstanceOf[ToClient])
  private class ToClientImpl extends ToClient {
    val logger = Logger.get
    private var clients = Set.empty[ClientOutbound]
    private var clientConfig: ClientConfig = null
    private var state: State = null
    private var ranges = Ranges.empty
    def connect(client: ClientOutbound): Unit = {
      logger.trace("CONNECT")
      clients += client
      if (clientConfig != null) {
        client.newConfig(clientConfig)
      }
      if (state != null) {
        client.newWork(state, ranges.parts)
      }
    }
    def newConfig(clientConfig: ClientConfig): Unit = {
      logger.trace("NEW CONFIG")
      this.clientConfig = clientConfig
      clients foreach { _.newConfig(clientConfig) }
    }
    def newWork(state: State, ranges: List[Ranges.Pair]): Unit = {
      logger.trace("NEW WORK")
      this.state = state
      this.ranges = Ranges(ranges)
      clients foreach { _.newWork(state, ranges) }
    }
    def assigned(id: UUID, range: Ranges.Pair): Unit = {
      logger.trace("ASSIGNED")
      ranges /= range
      clients foreach { _.assigned(id, range) }
    }
  }
  trait ToServer extends ServerOutbound {
    def connect: Unit
  }
  private class ToServerImpl(parent: ServerOutbound) extends ToServer {
    private var count = 0
    private var checkedIn = 0
    private var compute: Compute = null

    def connect = count += 1
    def sendCompute(compute: Compute) = {
      checkedIn += 1

      if (compute != null) {
        if (this.compute == null) {
          this.compute = compute
        } else {
          this.compute += compute
        }
      }
      check
    }

    private def check = {
      if (count == checkedIn) {
        parent.sendCompute(compute)
        compute = null
        checkedIn = 0
      }
    }
    def request(id: UUID, range: Ranges.Pair) = parent.request(id, range)
  }

  private class IIn(in: InputStream) extends InvokeIn(new DataInputStream(new BufferedInputStream(in))) {
    override def run = try { super.run } finally { System.exit(0) }
  }

  private class IOut(out: OutputStream) extends InvokeOut(new DataOutputStream(new BufferedOutputStream(out))) {
    override def handleException(e: Throwable) = {
      super.handleException(e)
      System.exit(0)
    }
  }

  private def toServer(upstreamOut: OutputStream): ToServer = {
    val out = new IOut(upstreamOut)
    out.start
    ActiveProxy[ToServer](new ToServerImpl(out.open[ServerOutbound](0)))
  }

  private def serverOutbound(serverOutbound: ServerOutbound)(downstreamIn: InputStream) = {
    val in = new IIn(downstreamIn)
    in + (0 -> serverOutbound)
    in.start
  }

  private def toClient(upstreamIn: InputStream): ToClient = {
    val tc = ActiveProxy[ToClient](new ToClientImpl)
    val in = new IIn(upstreamIn)
    in + (0 -> tc)
    in.start
    tc
  }

  private def clientOutbound(downstreamOut: OutputStream): ClientOutbound = {
    val out = new IOut(downstreamOut)
    out.start
    out.open[ClientOutbound](0)
  }

  def connect(host: String, port: Int, soTimeout: Int) = {
    val socket = new Socket(host, port)
    socket.setSoTimeout(soTimeout)
    new Bridge2(socket.getInputStream, socket.getOutputStream)
  }

  def apply(f: ClientOutbound => ServerOutbound) = new Bridge2(f)
}

class Bridge2 private (toClientInst: Bridge2.ToClient, toServerInst: Bridge2.ToServer) {
  private def this(in: InputStream, out: OutputStream) = this(Bridge2.toClient(in), Bridge2.toServer(out))
  private def this(toClientInst: Bridge2.ToClient, f: ClientOutbound => ServerOutbound) =
    this(toClientInst, ActiveProxy[Bridge2.ToServer](new Bridge2.ToServerImpl(f(toClientInst)).asInstanceOf[Bridge2.ToServer]))
  private def this(f: ClientOutbound => ServerOutbound) =
    this(Bridge2.getToClient, f)

  private def add(downstreamIn: InputStream, downstreamOut: OutputStream) = {
    toClientInst.connect(Bridge2.clientOutbound(downstreamOut))
    toServerInst.connect
    Bridge2.serverOutbound(toServerInst)(downstreamIn)
    this
  }

  def add(f: ServerOutbound => ClientOutbound) = {
    toServerInst.connect
    toClientInst.connect(f(toServerInst))
    this
  }

  def listen(port: Int, soTimeout: Int): Unit = {
    val log = Logger.get
    def loop(ssocket: ServerSocket, soTimeout: Int): Unit = {
      val socket = ssocket.accept
      log.trace(socket.toString)
      socket.setSoTimeout(soTimeout)
      add(socket.getInputStream, socket.getOutputStream)
      loop(ssocket, soTimeout)
    }
    loop(new ServerSocket(port), soTimeout)
  }
}

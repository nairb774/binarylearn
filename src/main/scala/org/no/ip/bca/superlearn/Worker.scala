package org.no.ip.bca.superlearn

import org.no.ip.bca.scala.FastRandom

object Client {
  def main(args: Array[String]): Unit = {
    val props = new Props(args(0))
    val bridge = Bridge2.connect(props.serverHost, props.serverPort, props.timeout)
    for (i <- 0 until props.processors) {
      bridge add { serverOutbound =>
        val client = new ClientActor(serverOutbound, props.memMapSource)
        client.start
        new ClientActorBridge(client)
      }
    }
  }
}

import java.util.UUID
import java.util.concurrent.RunnableFuture

import org.no.ip.bca.scala.{ LoggingSupport, Ranges }
import org.no.ip.bca.scala.utils.actor.ReActor

case class ClientConfig(sample: Double, steps: Int)

case class State(weights: Array[Double], hidden: Array[Double], visible: Array[Double]) {
  def w = visible.length
  def h = hidden.length
  lazy val transposedWeights = {
    val t = new Array[Double](weights.length)
    var m = 0
    var y = 0
    var x = 0
    val matrix = weights
    val mLen = matrix.length
    val w = this.w
    val h = this.h
    while (m < mLen) {
      t(x * h + y) = matrix(m)
      m += 1
      x += 1
      if (x == w) {
        y += 1
        x = 0
      }
    }
    t
  }
}

case class Compute(cd: Array[Long], vAct: Array[Long], hAct: Array[Long], count: Long) {
  def +(c: Compute) = {
    val newCd = sum(cd, c.cd)
    val newVAct = sum(vAct, c.vAct)
    val newHAct = sum(hAct, c.hAct)
    Compute(newCd, newVAct, newHAct, count + c.count)
  }

  def sum(a: Array[Long], b: Array[Long]) = {
    val aLength = a.length
    val c = new Array[Long](a.length)
    var i = 0
    while (i < aLength) {
      c(i) = a(i) + b(i)
      i += 1
    }
    c
  }
}

private object ClientActor {
  case class NewConfig(clientConfig: ClientConfig)
  case class NewWork(state: State, ranges: List[Ranges.Pair])
  case class Assigned(id: UUID, range: Ranges.Pair)
  case class Data(point: Long, data: Array[Byte])
  case class Finished(future: RunnableFuture[Compute])
}

class ClientActorBridge(client: ClientActor) extends ClientOutbound {
  import ClientActor._
  def assigned(id: UUID, range: Ranges.Pair) = client ! Assigned(id, range)
  def sendData(point: Long, data: Array[Byte]) = client ! Data(point, data)
  def newWork(state: State, ranges: List[Ranges.Pair]) = client ! NewWork(state, ranges)
  def newConfig(clientConfig: ClientConfig) = client ! NewConfig(clientConfig)
}

trait ServerOutbound {
  def sendCompute(compute: Compute): Unit
  def request(id: UUID, range: Ranges.Pair): Unit
}

class ClientActor(
    outbound: ServerOutbound,
    memMapSource: MemMapSource) extends ReActor with LoggingSupport {
  import ClientActor._
  private type PF = PartialFunction[Any, Unit]
  private val id = UUID.randomUUID
  trace(id)
  private val random = new FastRandom

  private var availableRanges = Ranges.empty
  private var clientConfig: ClientConfig = null
  private var state: State = null

  private var compute: Compute = null

  private val exec = java.util.concurrent.Executors.newSingleThreadExecutor()
  private var workingRange: Ranges.Pair = null
  private var work: RunnableFuture[Compute] = null
  private var workStart: Long = 0
  private var workRate = 50.0

  override def to(f: PF) = {
    reAct = f
    super.to(f)
  }

  private def finished(f: => Nothing): PF = {
    case Finished(future) =>
      if (future eq work) {
        workStart = System.nanoTime - workStart
        f
      }
  }

  private def workingState: PF =
    finished(to(finishedWorkingState)) orElse
      myAssigned(to(verifiedWorkingState))

  private def finishedWorkingState: PF =
    myAssigned(commit)

  private def verifiedWorkingState: PF =
    finished(commit) orElse
      otherAssigned

  private def awaitMatrixState: PF = {
    case NewConfig(clientConfig) =>
      this.clientConfig = clientConfig
    case NewWork(state, ranges) =>
      this.state = state
      availableRanges = Ranges(ranges)
      findWork
  }

  private def myAssigned(success: => Nothing): PF = {
    case Assigned(_id, range) =>
      availableRanges /= range
      if (id == _id) {
        success
      } else if (!(Ranges(range) & workingRange).isEmpty) {
        work.cancel(false)
        work = null
        to(awaitAssigned)
      }
  }

  private def awaitAssigned: PF = {
    case Assigned(_id, range) =>
      availableRanges /= range
      if (_id == id) {
        workingRange = range
        startWorker
        to(verifiedWorkingState)
      }
  }

  private def otherAssigned: PF = {
    case Assigned(_id, range) =>
      availableRanges /= range
      assert(id != _id, "We are not expecting any work requests to come back " + id + " " + range)
  }

  private def findWork = {
    if (availableRanges.isEmpty) {
      // Send matrix
      outbound.sendCompute(compute)
      state = null
      compute = null
      to(awaitMatrixState)
    } else {
      // Pick a random pair - and a random range in that pair. Hopefully we avoid colliding then.
      // We add 1 to the rate in case the rate drops to < 1 per second
      workingRange = pickRange(availableRanges)
      startWorker
      // Send request
      outbound.request(id, workingRange)
      to(workingState)
    }
  }

  private def pickRange(range: Ranges) = {
    // We add 1 to the rate in case the rate drops to < 1 per second
    val count = (workRate / clientConfig.sample).toLong + 1
    val rangeLength = range.length
    val p = range.parts.drop(if (rangeLength > 1) random.nextLong(rangeLength - 1).toInt else 0).head
    val window = p.end - p.start - count
    Ranges.Pair(if (window > 0) p.start + random.nextLong(window) else p.start, p.end).withMaxLength(count)
  }

  private def commit = {
    val compute = work.get
    workRate = workRate * 0.8 + 0.2 * compute.count / (workStart / 1E9)
    trace("RATE: {0}", workRate)
    if (this.compute == null) {
      this.compute = compute
    } else {
      this.compute += compute
    }
    findWork
  }

  var reAct: PF = awaitMatrixState

  private def startWorker: Unit = {
    // Start work
    val job = new MatrixJob(memMapSource, workingRange, state, clientConfig)
    //val worker = MatrixWorker.builder(dataManager.iter(workingRange.start, workingRange.end)).worker(state, clientConfig, transposedWeights)
    work = new MatrixWorkerFuture(job, future => this ! Finished(future))
    workStart = System.nanoTime
    exec.execute(work)
  }
}


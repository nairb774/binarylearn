package org.no.ip.bca.superlearn

import java.util.concurrent.RunnableFuture
import math._
import org.no.ip.bca.scala.{ Ranges, FastRandom }
import se.scalablesolutions.akka.actor.{ Actor, ActorRef }
import se.scalablesolutions.akka.dispatch.Future
import se.scalablesolutions.akka.remote.RemoteClient

object Client {
  def main(args: Array[String]): Unit = {
    ConfigHelper.bootstrap
    val remoteBridge = RemoteClient.actorFor("bridge", ConfigHelper.serverHost, ConfigHelper.serverPort)
    val localBridge = Actor.actorOf(new Bridge(remoteBridge)).start
    for (count <- 0 until ConfigHelper.processors) {
      val client = Actor.actorOf(new ClientActor(localBridge, ConfigHelper.memMapSource))
      localBridge.startLink(client)
    }
  }
}

case class ClientConfig(sample: Double, steps: Int)

case class State(weights: Matrix.Immutable[Side.H, Side.V], hidden: Matrix.ImmutableV[Side.H], visible: Matrix.ImmutableV[Side.V]) {
  def v = visible.columns
  def h = hidden.columns
  lazy val transposedWeights = weights.T
  lazy val toMutable = MutableState(weights.toMutable, hidden.toMutable, visible.toMutable)
}

case class MutableState(weights: Matrix.Mutable[Side.H, Side.V], hidden: Matrix.MutableV[Side.H], visible: Matrix.MutableV[Side.V]) {
  def v = visible.columns
  def h = hidden.columns
  lazy val transposedWeights = weights.T
}

case class Compute(cd: Matrix.Immutable[Side.H, Side.V], vAct: Matrix.ImmutableV[Side.V], hAct: Matrix.ImmutableV[Side.H], count: Long) {
  def +(c: Compute) = {
    val newCd = c.cd + cd
    val newVAct = c.vAct + vAct
    val newHAct = c.hAct + hAct
    Compute(newCd, newVAct, newHAct, count + c.count)
  }
}

object ClientActor {
  final case class NewConfig(clientConfig: ClientConfig)
  final case class NewState(state: State)
  final case class NewRanges(ranges: List[Ranges.Pair])
  final case class Assigned(id: String, range: Ranges.Pair)
}

class ClientActor(
  parent: ActorRef,
  memMapSource: MemMapSource) extends Actor {
  import ClientActor._
  private val random = new FastRandom

  private var availableRanges = Ranges.empty
  private var clientConfig: Option[ClientConfig] = None
  private var state: Option[State] = None

  private var compute: Option[Compute] = None

  private var workingRange: Option[Ranges.Pair] = None
  private var work: Option[WorkerActor.Work] = None
  private var workFuture: Option[Future[ServerActor.RecieveCompute]] = None
  private var workStart: Long = 0
  private var workRate = 5.0

  private var matrixWorker: Option[ActorRef] = None

  private def finished(onFinish: => Unit): Receive = {
    case ServerActor.RecieveCompute(_) =>
      if (workFuture.get.isCompleted) {
        workStart = System.nanoTime - workStart
        onFinish
      }
  }

  private def workingState: Receive =
    finished(become(finishedWorkingState)) orElse
      myAssigned(become(verifiedWorkingState))

  private def finishedWorkingState: Receive =
    myAssigned(commit)

  private def verifiedWorkingState: Receive =
    finished(commit) orElse
      otherAssigned

  private def awaitMatrixState: Receive = {
    case NewConfig(clientConfig) =>
      this.clientConfig = Some(clientConfig)
    case NewState(state) =>
      this.state = Some(state)
    case NewRanges(ranges) =>
      availableRanges = Ranges(ranges)
      findWork
  }

  private def myAssigned(success: => Unit): Receive = {
    case Assigned(id, range) =>
      availableRanges /= range
      if (self.uuid == id) {
        assert(range == workingRange.get)
        success
      } else if (!(Ranges(range) & workingRange.get).isEmpty) {
        work foreach { _.cancel }
        work = None
        if (availableRanges.isEmpty) {
          findWork
        } else {
          become(awaitAssigned)
        }
      }
  }

  private def awaitAssigned: Receive = {
    case Assigned(id, range) =>
      availableRanges /= range
      if (self.uuid == id) {
        workingRange = Some(range)
        startWorker
        become(verifiedWorkingState)
      } else if (availableRanges.isEmpty) {
        findWork
      }
    case ServerActor.RecieveCompute(_) => // ignore
  }

  private def otherAssigned: Receive = {
    case Assigned(id, range) =>
      availableRanges /= range
      assert(self.uuid != id, "We are not expecting any work requests to come back " + id + " " + range)
  }

  private def findWork = {
    if (availableRanges.isEmpty) {
      // Send matrix
      compute foreach { parent ! ServerActor.RecieveCompute(_) }
      compute = None
      become(awaitMatrixState)
    } else {
      // Pick a random pair - and a random range in that pair. Hopefully we avoid colliding then.
      // We add 1 to the rate in case the rate drops to < 1 per second
      workingRange = Some(pickRange(availableRanges))
      startWorker
      // Send request
      parent ! ServerActor.Request(workingRange.get)
      become(workingState)
    }
  }

  private def pickRange(range: Ranges) = {
    // We add 1 to the rate in case the rate drops to < 1 per second
    val count = (workRate / clientConfig.get.sample).toLong + 1
    val rangeLength = range.length
    val p = range.parts.drop(if (rangeLength > 1) random.nextLong(rangeLength - 1).toInt else 0).head
    val window = p.end - p.start - count
    Ranges.Pair(if (window > 0) p.start + random.nextLong(window) else p.start, p.end).withMaxLength(count)
  }

  private def commit = {
    val compute = workFuture.get.await.result.get.compute
    workRate = workRate * 0.8 + 0.2 * compute.count / (workStart / 1E9)
    log.info("RATE: " + workRate)
    this.compute = this.compute map { _ + compute } orElse Some(compute)
    findWork
  }

  override def init = {
    matrixWorker = Some(self.spawnLink[WorkerActor])
    parent ! ServerActor.Connect
  }
  def receive = awaitMatrixState

  private def startWorker: Unit = {
    // Start work
    work = Some(new WorkerActor.Work(MatrixWorker.builder(memMapSource.iter(workingRange.get)).worker(state.get.toMutable, clientConfig.get)))
    workStart = System.nanoTime
    workFuture = Some(matrixWorker.get !!! (WorkerActor.Run(work.get), 5000))
  }
}


package org.no.ip.bca.superlearn

object Client {
    def main(args: Array[String]): Unit = {
        val props = new Props(args(0))
        val melder = Bridger.connect(props)
        for (i <- 0 until props.processors) {
            val client = new ClientActor(melder(), props.dataManager)
            client.start
            melder.connect(new ClientActorBridge(client))
        }
    }
}

import java.util.UUID
import java.util.concurrent.RunnableFuture

import org.no.ip.bca.scala.Ranges
import org.no.ip.bca.scala.utils.actor.ReActor

case class State(weights: Array[Double], hidden: Array[Double], visible: Array[Double], sample: Double, steps: Int) {
    def w = visible.length
    def h = hidden.length
    def transposedWeights = {
        val t = new Array[Double](weights.length)
        UtilMethods.transpose(weights, w, h, t)
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
  case class NewWork(state: State, ranges: Ranges.Pair*)
  case class Assigned(id: UUID, range: Ranges.Pair)
  case class Data(point: Long, data: Array[Byte])
  case class Finished(future: RunnableFuture[Compute])
}

class ClientActorBridge(client: ClientActor) extends ClientOutbound {
  import ClientActor._
  def assigned(id: UUID, range: Ranges.Pair) = client ! Assigned(id, range)
  def sendData(point: Long, data: Array[Byte]) = client ! Data(point, data)
  def newWork(state: State, ranges: Ranges.Pair*) = client ! NewWork(state, ranges: _*)
}

trait ServerOutbound {
  def requestPoint(point: Long): Unit
  def sendCompute(compute: Compute): Unit
  def request(id: UUID, range: Ranges.Pair): Unit
}

class ClientActor(
    outbound: ServerOutbound,
    dataManager: DataManager
    ) extends ReActor {
  import ClientActor._
  private type PF = PartialFunction[Any, Unit]
  private val id = UUID.randomUUID
  private val random = new FastRandom
  
  private var availableRanges = Ranges.empty
  private var state: State = null
  private var transposedWeights: Array[Double] = null
  
  private var compute: Compute = null
  
  private var workingRange: Ranges.Pair = null
  private var work: RunnableFuture[Compute] = null
  
  override def to(f: PF) = {
    reAct = f
    super.to(f)
  }
  
  private def finished(f: => Nothing): PF = {
    case Finished(future) => if (future eq work) f
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
    case NewWork(state, ranges) =>
      this.state = state
      this.transposedWeights = state.transposedWeights
      availableRanges = Ranges(ranges)
      findWork
  }
  
  private def awaitDataState: PF = ({
    case Data(point, data) =>
      dataManager.write(point, data)
      findWork
  }: PF) orElse
     otherAssigned
  
  private def myAssigned(success: => Nothing): PF = {
    case Assigned(_id, range) =>
      availableRanges /= range
      if (id == _id) {
        success
      } else if (!(Ranges(range) & workingRange).isEmpty) {
        work.cancel(false)
        work = null
        findWork
      }
  }
  
  private def otherAssigned: PF = {
    case Assigned(_id, range) =>
      availableRanges /= range
      assert(id != _id, "We are not expecting any works requests to come back")
  }
  
  private def findWork = {
    if (availableRanges.isEmpty) {
      // Send matrix
      outbound.sendCompute(compute)
      state = null
      transposedWeights = null
      
      compute = null
      
      to(awaitMatrixState)
    }
    val pickFrom = availableRanges & dataManager.ranges
    if (pickFrom.isEmpty) {
      val pair = availableRanges.head
      val point = pair.start + random.nextLong(pair.end - pair.start)
      outbound.requestPoint(point)
      
      to(awaitDataState)
    } else {
      // Pick
      workingRange = pickFrom.head.withMaxLength(10240)
      // Start work
      val worker = new MatrixWorker(state, transposedWeights, workingRange, dataManager)
      work = new MatrixWorkerFuture(worker, future => this ! Finished(future))
      val thread = new Thread(work)
      thread.setDaemon(true)
      thread.setName("Matrix math thread")
      thread.start
      // Send request
      outbound.request(id, workingRange)
      to(workingState)
    }
  }
  
  private def commit = {
    val compute = work.get
    if (this.compute == null) {
      this.compute = compute
    } else {
      this.compute += compute
    }
    findWork
  }
  
  var reAct = awaitMatrixState
}


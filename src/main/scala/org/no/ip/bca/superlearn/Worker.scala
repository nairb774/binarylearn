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

object Matrix {
    val EMPTY = Matrix(0, 0, Array(), null, null)
}
case class Matrix(w: Int, h: Int, m: Array[Double], hidden: Array[Double], visible: Array[Double])

private object ClientActor {
  case class NewWork(matrix: Matrix, ranges: Ranges.Pair*)
  case class Assigned(id: UUID, range: Ranges.Pair)
  case class Data(point: Long, data: Array[Byte])
  case class Finished(future: RunnableFuture[(Matrix, Long)])
}

class ClientActorBridge(client: ClientActor) extends ClientOutbound {
  import ClientActor._
  def assigned(id: UUID, range: Ranges.Pair) = client ! Assigned(id, range)
  def sendData(point: Long, data: Array[Byte]) = client ! Data(point, data)
  def newWork(matrix: Matrix, ranges: Ranges.Pair*) = client ! NewWork(matrix, ranges: _*)
}

trait ServerOutbound {
  def requestPoint(point: Long): Unit
  def sendMatrix(matrix: Matrix, count: Long): Unit
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
  private var matrix: Matrix = null
  private var transpose: Matrix = null
  private var nextMatrix: Matrix = null
  private var nextCount: Long = 0
  
  private var workingRange: Ranges.Pair = null
  private var work: RunnableFuture[(Matrix, Long)] = null
  
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
    case NewWork(matrix, ranges) =>
      availableRanges = Ranges(ranges)
      val m = matrix.m
      val t = new Array[Double](m.length)
      val w = matrix.w
      val h = matrix.h
      UtilMethods.transpose(m, w, h, t)
      
      this.matrix = matrix
      this.transpose = Matrix(h, w, t, null, null)
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
      if (nextCount == 0) {
        outbound.sendMatrix(Matrix.EMPTY, 0)
      } else {
        outbound.sendMatrix(nextMatrix, nextCount)
      }
      matrix = null
      transpose = null
      nextMatrix = null
      nextCount = 0
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
      val worker = new MatrixWorker(matrix, transpose, workingRange, dataManager)
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
    val (matrix, count) = work.get
    if (nextMatrix == null) {
      nextMatrix = matrix
    } else {
      UtilMethods.sum(matrix.m, nextMatrix.m)
    }
    nextCount += count
    findWork
  }
  
  var reAct = awaitMatrixState
}


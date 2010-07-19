package org.no.ip.bca.superlearn

object Worker {
  import java.io._
  import java.nio._
  import java.util._
  
  def main(args: Array[String]): Unit = {
    val props = new Props(args(0))
    val dataManager = props.dataManager
    val matrix = props.matrix
    
    val server = new ServerActor(dataManager, matrix)
    server.start
    /*for (i <- 0 until Runtime.getRuntime.availableProcessors) {
      val serverOutbound = new ServerActorBridge(server)
      val client = new ClientActor(serverOutbound, dataManager)
      client.start
      serverOutbound.client = new ClientActorBridge(client)
    }*/
    ()
  }
}

import java.util.UUID
import java.util.concurrent.RunnableFuture

import org.no.ip.bca.scala.Ranges
import org.no.ip.bca.scala.utils.actor.ReActor

case class Matrix(w: Int, h: Int, m: Array[Double])

private object ClientActor {
  case class NewWork(matrix: Matrix, ranges: Ranges.Pair*)
  case class NewRanges(ranges: Ranges.Pair*)
  case class Assigned(id: UUID, range: Ranges.Pair)
  case class Data(point: Long, data: Array[Byte])
  case class Finished(future: RunnableFuture[Matrix])
}

class ClientActorBridge(client: ClientActor) extends ClientOutbound {
  import ClientActor._
  def newRanges(ranges: Ranges.Pair*) = client ! NewRanges(ranges: _*)
  def assigned(id: UUID, range: Ranges.Pair) = client ! Assigned(id, range)
  def sendData(point: Long, data: Array[Byte]) = client ! Data(point, data)
  def newWork(matrix: Matrix, ranges: Ranges.Pair*) = client ! NewWork(matrix, ranges: _*)
}

trait ServerOutbound {
  def requestPoint(point: Long): Unit
  def sendMatrix(matrix: Matrix, count: Long): Unit
  def request(id: UUID, range: Ranges.Pair)
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
  private var nextMatrix: Array[Double] = null
  
  private var workingRange: Ranges.Pair = null
  private var work: RunnableFuture[Matrix] = null
  
  override def to(f: PF) = {
    reAct = f
    super.to(f)
  }
  
  private def newRanges: PF = {
    case NewRanges(ranges) => availableRanges |= Ranges(ranges)
  }
  
  private def finished(f: => Nothing): PF = {
    case Finished(future) => if (future eq work) f
  }
  
  private def workingState: PF =
      finished(to(finishedWorkingState)) orElse
      myAssigned(to(verifiedWorkingState)) orElse
      newRanges
  
  private def finishedWorkingState: PF =
      myAssigned(commit) orElse
      newRanges
  
  private def verifiedWorkingState: PF =
      finished(commit) orElse
      otherAssigned orElse
      newRanges
  
  private def awaitMatrixState: PF = {
    case NewWork(matrix, ranges) =>
      availableRanges = Ranges(ranges)
      val m = matrix.m
      val t = new Array[Double](m.length)
      val w = matrix.w
      val h = matrix.h
      UtilMethods.transpose(m, w, h, t)
      
      this.matrix = matrix
      this.transpose = Matrix(h, w, t)
      findWork
  }
  
  private def awaitDataState: PF = ({
    case Data(point, data) =>
      dataManager.write(point, data)
      findWork
  }: PF) orElse
     otherAssigned orElse
     newRanges
  
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
      if (nextMatrix == null) {
        outbound.sendMatrix(Matrix(0, 0, Array()), 0)
      } else {
        val m = new Array[Double](nextMatrix.length - 1)
        System.arraycopy(nextMatrix, 0, m, 0, m.length)
        outbound.sendMatrix(Matrix(matrix.w, matrix.h, m), nextMatrix(m.length).toLong)
        matrix = null
        transpose = null
        nextMatrix = null
      }
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
    val m = work.get.m
    if (nextMatrix == null) {
      nextMatrix = m.clone
    } else {
      UtilMethods.sum(m, nextMatrix)
    }
    findWork
  }
  
  var reAct = awaitMatrixState
}


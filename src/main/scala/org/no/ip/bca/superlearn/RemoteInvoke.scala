package org.no.ip.bca.superlearn

import java.io.{ObjectInputStream, ObjectOutputStream}
import java.lang.reflect.{InvocationHandler, Method, Proxy}
import java.util.IdentityHashMap

import org.no.ip.bca.scala.utils.actor.ReActor

private object InvokeTypes {
  case class MethodSig(name: String, types: Array[String])
  case class Invoke(id: Int, method: MethodSig, args: Array[AnyRef])
  val primitives = Map (
      "boolean" -> classOf[Boolean],
      "byte" -> classOf[Byte],
      "short" -> classOf[Short],
      "char" -> classOf[Char],
      "int" -> classOf[Int],
      "long" -> classOf[Long],
      "float" -> classOf[Float],
      "double" -> classOf[Double]
  )
}

class InvokeOut(out: ObjectOutputStream) extends ReActor {
  import InvokeTypes._
  private class InvokeHandler(id: Int) extends InvocationHandler {
    private val methods = new IdentityHashMap[Method, MethodSig]
    
    private def getSig(method: Method) = methods.synchronized {
      var sig = methods.get(method)
      if (sig == null) {
        sig = MethodSig(method.getName, method.getParameterTypes map {_.getName})
        methods.put(method, sig)
      }
      sig
    }
    
    def invoke(proxy: AnyRef, method: Method, args: Array[AnyRef]): AnyRef = {
      InvokeOut.this ! Invoke(id, getSig(method), args)
      null
    }
  }
    
  def open[T](id: Int)(implicit m: ClassManifest[T]): T = {
    val handler = new InvokeHandler(id)
    val clazz = m.erasure
    Proxy.newProxyInstance(clazz.getClassLoader, Array(clazz), handler).asInstanceOf[T]
  }
  override def init = {
    super.init
    out.flush
  }
  def reAct = {
    case Invoke(id, sig, args) =>
      out.writeInt(id)
      out.writeObject(sig)
      out.writeUnshared(args)
      out.flush
  }
  override def doStop = {
    super.doStop
    out.writeInt(-1)
    out.close
  }
}

class InvokeIn(in: ObjectInputStream) extends Runnable {
  import InvokeTypes._
  private class Invoker[T <: AnyRef](obj: T) extends ReActor {
    private val methods = new IdentityHashMap[MethodSig, Method]
    
    def reAct = {
      case Invoke(_, methodSig, args) => lookupMethod(methodSig).invoke(obj, args: _*)
    }
    
    private def lookupMethod(methodSig: MethodSig) = {
      var method = methods.get(methodSig)
      if (method == null) {
        val clazz = obj.getClass
        val classLoader = clazz.getClassLoader
        val paramTypes = try {
          methodSig.types map primitives.withDefault(Class.forName(_, true, classLoader))
        } catch {
            case e =>
              e.printStackTrace
              throw e
        }
        method = clazz.getMethod(methodSig.name, paramTypes: _*)
        methods.put(methodSig, method)
      }
      method
    }
    
    override def doStop = methods.clear
  }
  
  private var map = Map.empty[Int, Invoker[_]]
  def start = new Thread(this).start
  def +[T <: AnyRef](pair: (Int, T)) = map += (pair._1 -> new Invoker(pair._2))
  def run: Unit = {
      map.values foreach {_.start}
      try {
          run0(map)
      } finally {
          map.values foreach {_.stop}
          in.close
      }
  }
  
  private def run0(map: Map[Int, Invoker[_]]): Unit = {
      val id = in.readInt
      if (id >= 0) {
          val sig = in.readObject.asInstanceOf[MethodSig]
          val args = in.readUnshared.asInstanceOf[Array[AnyRef]]
          map(id) ! Invoke(id, sig, args)
          run0(map)
      }
  }
}



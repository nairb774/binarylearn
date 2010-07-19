package org.no.ip.bca.superlearn

import org.no.ip.bca.scala.utils.actor.ReActor

private object InvokeTypes {
  case class MethodSig(name: String, types: Array[String])
  case class Invoke(id: Int, method: MethodSig, args: Array[AnyRef])
}

class InvokeOut(out: java.io.ObjectOutputStream) extends ReActor {
  import InvokeTypes._
  private class InvokeHandler(id: Int) extends java.lang.reflect.InvocationHandler {
    import java.lang.reflect.Method
    private val methods = new java.util.IdentityHashMap[Method, MethodSig]
    
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
    
  def open[T](id: Int)(implicit m: Manifest[T]): T = {
    val handler = new InvokeHandler(id)
    val clazz = m.erasure
    java.lang.reflect.Proxy.newProxyInstance(clazz.getClassLoader, Array(clazz), handler).asInstanceOf[T]
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

class InvokeIn(in: java.io.ObjectInputStream) extends Runnable {
  import InvokeTypes._
  private class Invoker[T <: AnyRef](obj: T) extends ReActor {
    import java.lang.reflect.Method
    private val methods = new java.util.IdentityHashMap[MethodSig, Method]
    
    def reAct = {
      case Invoke(_, methodSig, args) => lookupMethod(methodSig).invoke(obj, args: _*)
    }
    
    private def lookupMethod(methodSig: MethodSig) = {
      var method = methods.get(methodSig)
      if (method == null) {
        val clazz = obj.getClass
        val classLoader = clazz.getClassLoader
        val paramTypes = methodSig.types map { Class.forName(_, true, classLoader) }
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
    val map0 = map
    do {
      val id = in.readInt
      if (id < 0) {
        in.close
        map0.values foreach {_.stop}
        return
      } else {
        val sig = in.readObject.asInstanceOf[MethodSig]
        val args = in.readUnshared.asInstanceOf[Array[AnyRef]]
        map0(id) ! Invoke(id, sig, args)
      }
    } while (true)
  }
}



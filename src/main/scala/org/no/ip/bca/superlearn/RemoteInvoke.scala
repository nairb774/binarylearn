package org.no.ip.bca.superlearn

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInput, DataOutputStream, ObjectInputStream, ObjectOutputStream}
import java.lang.reflect.{InvocationHandler, Method, Proxy}
import java.util.IdentityHashMap

import org.no.ip.bca.scala.utils.actor.ReActor

private object InvokeTypes {
  case class MethodSig(id: Int, name: String, types: Array[String])
  
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

object ActiveProxy {
    def apply[T <: AnyRef](obj: T)(implicit m: ClassManifest[T]): T = {
        val handler = new ActiveProxy(obj)
        handler.start
        val clazz = m.erasure
        Proxy.newProxyInstance(clazz.getClassLoader, Array(clazz), handler).asInstanceOf[T]
    }
}

private class ActiveProxy[T <: AnyRef](obj: T) extends InvocationHandler with ReActor {
    private case class I(method: Method, args: Array[AnyRef])
    def invoke(proxy: AnyRef, method: Method, args: Array[AnyRef]): AnyRef = {
      method.getName match {
        case "equals" if args.length == 1 =>
          (this == args(0)).asInstanceOf[AnyRef]
        case "hashCode" if args.length == 0 =>
          (this.hashCode).asInstanceOf[AnyRef]
        case "toString" if args.length == 0 =>
          this.toString
        case _ =>
          this ! I(method, args)
          null
      }
    }
    
    val reAct: PartialFunction[Any, Unit] = {
        case I(method, args) => method.invoke(obj, args: _*)
    }
}

class InvokeOut(out: DataOutputStream) extends ReActor {
  import InvokeTypes._
  private case class Call(sig: MethodSig, args: Array[Byte])
  private case class NewMethod(sig: MethodSig, sigBytes: Array[Byte])
  
  private class InvokeHandler(id: Int) extends InvocationHandler {
    private val methods = new IdentityHashMap[Method, MethodSig]
    
    private def getSig(method: Method) = methods.synchronized {
      var sig = methods.get(method)
      if (sig == null) {
        sig = MethodSig(id, method.getName, method.getParameterTypes map {_.getName})
        methods.put(method, sig)
        InvokeOut.this ! NewMethod(sig, toBytes(sig))
      }
      sig
    }
    
    def invoke(proxy: AnyRef, method: Method, args: Array[AnyRef]): AnyRef = {
      method.getName match {
        case "equals" if args.length == 1 =>
          (this == args(0)).asInstanceOf[AnyRef]
        case "hashCode" if args.length == 0 =>
          (this.hashCode).asInstanceOf[AnyRef]
        case "toString" if args.length == 0 =>
          this.toString
        case _ =>
          InvokeOut.this ! Call(getSig(method), toBytes(args))
          null
      }
    }
  }
  
  private var methodId = 1
  private val methods = new IdentityHashMap[MethodSig, Int]
  
  private def toBytes(obj: AnyRef): Array[Byte] = {
    val baos = new ByteArrayOutputStream
    val oos = new ObjectOutputStream(baos)
    oos.writeObject(obj)
    oos.close
    baos.toByteArray
  }
  
  def open[T](id: Int)(implicit m: ClassManifest[T]): T = {
    val handler = new InvokeHandler(id)
    val clazz = m.erasure
    Proxy.newProxyInstance(clazz.getClassLoader, Array(clazz), handler).asInstanceOf[T]
  }
  
  def reAct = {
    case Call(sig, args) =>
      out.writeInt(methods.get(sig))
      out.writeInt(args.length)
      out.write(args)
      out.flush
    case NewMethod(sig, sigBytes) =>
      methods.put(sig, methodId)
      out.writeInt(methodId)
      out.writeInt(sigBytes.length)
      out.write(sigBytes)
      methodId += 1
      // No flush since an invocation should be following soon
  }
  
  override def doStop = {
    super.doStop
    out.writeInt(0)
    out.close
  }
}

class InvokeIn(in: DataInput) extends Runnable {
  import InvokeTypes._
  private case class Call(method: MethodSig, args: Array[Byte])
  
  private class Invoker[T <: AnyRef](obj: T) extends ReActor {
    private val methods = new IdentityHashMap[MethodSig, Method]
    private val clazz = obj.getClass
    private val typeLookup = {
        val classLoader = clazz.getClassLoader
        primitives.withDefault(Class.forName(_, true, classLoader))
    }
    
    def reAct = {
      case Call(methodSig, args) => lookupMethod(methodSig).invoke(obj, fromBytes[Array[AnyRef]](args): _*)
    }
    
    private def lookupMethod(methodSig: MethodSig) = {
      var method = methods.get(methodSig)
      if (method == null) {
        method = clazz.getMethod(methodSig.name, methodSig.types map typeLookup: _*)
        methods.put(methodSig, method)
      }
      method
    }
    
    override def doStop = methods.clear
  }
  
  private var map = Map.empty[Int, Invoker[_]]
  private var methods = Map.empty[Int, (Array[Byte]) => Unit]
  
  private def fromBytes[T](bytes: Array[Byte]): T = {
    val bais = new ByteArrayInputStream(bytes)
    val ois = new ObjectInputStream(bais)
    ois.readObject().asInstanceOf[T]
  } 
  def start = new Thread(this).start
  def +[T <: AnyRef](pair: (Int, T)) = map += (pair._1 -> new Invoker(pair._2))
  def run: Unit = {
      map.values foreach {_.start}
      try {
          run0(map)
      } finally {
          map.values foreach {_.stop}
      }
  }
  
  private def run0(map: Map[Int, Invoker[_]]): Unit = {
    val methodId = in.readInt
    if (methodId != 0) {
      methods.get(methodId) match {
        case Some(f) =>
          val bytes = new Array[Byte](in.readInt)
          in.readFully(bytes)
          f(bytes)
        case None =>
          val bytes = new Array[Byte](in.readInt)
          in.readFully(bytes)
          val sig = fromBytes[MethodSig](bytes)
          val obj = map(sig.id)
          methods += methodId -> { obj ! Call(sig, _) }
      }
      run0(map)
    }
  }
}


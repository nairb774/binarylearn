package org.no.ip.bca.superlearn

import javax.management.ObjectName
import java.lang.management.ManagementFactory
import scala.reflect.BeanProperty

case class ConfigState(
  sample: Double,
  steps: Int,
  wMomentum: Double,
  vMomentum: Double,
  hMomentum: Double,
  epsilon: Double,
  weightcost: Double)

object Config {
  val config = {
    val config = new Config()
    ManagementFactory.getPlatformMBeanServer.registerMBean(config, new ObjectName("org.no.ip.bca.superlearn.Config:name=config"))
    config
  }
}

trait ConfigMBean {
  def getSample: Double
  def setSample(sample: Double): Unit
  def getSteps: Int
  def setSteps(steps: Int): Unit
  def getWMomentum: Double
  def setWMomentum(wMomentum: Double): Unit
  def getVMomentum: Double
  def setVMomentum(vMomentum: Double): Unit
  def getHMomentum: Double
  def setHMomentum(hMomentum: Double): Unit
  def getEpsilon: Double
  def setEpsilon(epsilon: Double): Unit
  def getWeightcost: Double
  def setWeightcost(weightcost: Double): Unit
}

class Config private[Config] () extends ConfigMBean {
  private var config = ConfigState(
    sample = 1.0,
    steps = 1,
    wMomentum = 0.8,
    vMomentum = 0.8,
    hMomentum = 0.8,
    epsilon = 0.001,
    weightcost = 0.0001
    )
  def getSample = synchronized { config.sample }
  def setSample(sample: Double) = synchronized { config = config copy (sample = sample) }
  def getSteps = synchronized { config.steps }
  def setSteps(steps: Int) = synchronized { config = config copy (steps = steps) }
  def getWMomentum = synchronized { config.wMomentum }
  def setWMomentum(wMomentum: Double) = synchronized { config = config copy (wMomentum = wMomentum) }
  def getVMomentum = synchronized { config.vMomentum }
  def setVMomentum(vMomentum: Double) = synchronized { config = config copy (vMomentum = vMomentum) }
  def getHMomentum = synchronized { config.hMomentum }
  def setHMomentum(hMomentum: Double) = synchronized { config = config copy (hMomentum = hMomentum) }
  def getEpsilon = synchronized { config.epsilon }
  def setEpsilon(epsilon: Double) = synchronized { config = config copy (epsilon = epsilon) }
  def getWeightcost = synchronized { config.weightcost }
  def setWeightcost(weightcost: Double) = synchronized { config = config copy (weightcost = weightcost) }
  def state = synchronized { config }
  def state_=(config: ConfigState) = synchronized { this.config = config }
  def clientConfig = synchronized { ClientConfig(config.sample, config.steps) }
}
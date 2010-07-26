package org.no.ip.bca.superlearn

import java.lang.management.ManagementFactory
import javax.management.ObjectName

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

class Config private[Config]() {
    private var config = ConfigState(
            sample = 0.05,
            steps = 1,
            wMomentum = 0.8,
            vMomentum = 0.8,
            hMomentum = 0.8,
            epsilon = 0.001,
            weightcost = 0.0001
        )
    def getSample = synchronized { config.sample }
    def setSample(sample: Double) = synchronized { config = config copy (sample = sample) }
    def getSteps = synchronized { config.sample }
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
    def getState = synchronized { config }
    def setState(config: ConfigState) = synchronized { this.config  = config }
}
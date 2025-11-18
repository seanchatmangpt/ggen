/**
 * Platform Integration Layer
 * Connects Quantum Security, 3D Visualization, and Agent Swarms
 *
 * This is the glue that ties all 2028+ innovations together
 */

import { SwarmAPI, getSwarmAPI } from './agents/swarm-api'
import { QuantumCryptoService, getQuantumCryptoService } from './quantum-crypto'
import { Visualization3D } from './three-d-visualization'

/**
 * Integrated Platform Manager
 * Single entry point for all 2028+ systems
 */
export class PlatformIntegration {
  private swarmAPI: SwarmAPI
  private quantumCrypto: QuantumCryptoService
  private visualizations: Map<string, Visualization3D> = new Map()
  private eventLog: any[] = []
  private errorLog: any[] = []
  private config: Map<string, any> = new Map()

  constructor() {
    this.swarmAPI = getSwarmAPI()
    this.quantumCrypto = getQuantumCryptoService()

    this.setupIntegration()
  }

  /**
   * Setup cross-component integration
   */
  private setupIntegration(): void {
    // Connect swarm events to logging
    this.setupEventLogging()
    // Setup error handling
    this.setupErrorHandling()
    // Setup persistence
    this.setupPersistence()
  }

  /**
   * Create swarm with quantum-encrypted communication
   */
  createSecureSwarm(
    swarmId: string,
    agentConfigs: any[],
    config?: any
  ): any {
    try {
      // Create swarm
      const swarm = this.swarmAPI.createSwarmWithAgents(
        swarmId,
        agentConfigs,
        config
      )

      if (!swarm) {
        throw new Error('Failed to create swarm')
      }

      // Generate quantum keys for swarm
      const swarmKeyPair = this.quantumCrypto.generateKeyPair({
        purpose: 'communication',
        userId: swarmId,
        expiryDays: 90,
      })

      // Store swarm config
      this.config.set(swarmId, {
        keyId: swarmKeyPair.keyId,
        createdAt: new Date(),
        agents: agentConfigs.length,
      })

      // Log creation
      this.logEvent('swarm_created', { swarmId, agents: agentConfigs.length })

      return {
        swarm,
        keyId: swarmKeyPair.keyId,
        secure: true,
      }
    } catch (error) {
      this.logError('swarm_creation_failed', error)
      throw error
    }
  }

  /**
   * Create 3D visualization for swarm
   */
  createSwarmVisualization(
    swarmId: string,
    canvasId: string,
    mode: 'process' | 'code' | 'agent-swarm' = 'agent-swarm'
  ): Visualization3D | null {
    try {
      const viz = new Visualization3D(canvasId, {
        width: 1200,
        height: 800,
        lightingPreset: 'studio',
        autoRotate: mode === 'agent-swarm',
        vrEnabled: true,
      })

      // Get swarm agents and visualize
      const agents = this.swarmAPI.listAgents(swarmId)

      // Convert agents to 3D nodes
      agents.forEach((agent, index) => {
        const angle = (index / agents.length) * Math.PI * 2
        const radius = 3

        viz.addNode({
          id: agent.getId(),
          position: [
            Math.cos(angle) * radius,
            0,
            Math.sin(angle) * radius,
          ],
          size: 1,
          color: agent.getRole() === 'coordinator' ? '#FF6B6B' : '#4ECDC4',
          label: agent.getName(),
          type: agent.getRole() === 'coordinator' ? 'gateway' : 'task',
          metadata: { agentId: agent.getId(), role: agent.getRole() },
        })
      })

      this.visualizations.set(swarmId, viz)
      this.logEvent('visualization_created', { swarmId, agents: agents.length })

      return viz
    } catch (error) {
      this.logError('visualization_creation_failed', error)
      return null
    }
  }

  /**
   * Sync visualization with swarm state
   */
  syncVisualizationWithSwarm(swarmId: string): void {
    try {
      const viz = this.visualizations.get(swarmId)
      if (!viz) return

      const agents = this.swarmAPI.listAgents(swarmId)

      // Update node colors based on agent health
      agents.forEach((agent) => {
        const health = agent.getHealthScore()
        const color =
          health > 70
            ? '#10b981' // Green
            : health > 50
              ? '#f59e0b' // Yellow
              : '#ef4444' // Red

        // Update would happen here in a real implementation
        this.logEvent('node_synced', {
          agentId: agent.getId(),
          health,
          color,
        })
      })
    } catch (error) {
      this.logError('visualization_sync_failed', error)
    }
  }

  /**
   * Encrypt swarm communication
   */
  encryptSwarmMessage(
    swarmId: string,
    message: any
  ): { encrypted: any; keyId: string } | null {
    try {
      const swarmConfig = this.config.get(swarmId)
      if (!swarmConfig) {
        throw new Error(`Swarm ${swarmId} not found`)
      }

      const keyPair = this.quantumCrypto.getKeyPair(swarmConfig.keyId)
      if (!keyPair) {
        throw new Error('Quantum key pair not found')
      }

      const encrypted = this.quantumCrypto.encrypt(
        JSON.stringify(message),
        keyPair.publicKey,
        keyPair.keyId
      )

      this.logEvent('message_encrypted', {
        swarmId,
        messageSize: JSON.stringify(message).length,
      })

      return { encrypted, keyId: keyPair.keyId }
    } catch (error) {
      this.logError('message_encryption_failed', error)
      return null
    }
  }

  /**
   * Decrypt swarm message
   */
  decryptSwarmMessage(
    swarmId: string,
    encryptedPayload: any
  ): any | null {
    try {
      const swarmConfig = this.config.get(swarmId)
      if (!swarmConfig) {
        throw new Error(`Swarm ${swarmId} not found`)
      }

      const keyPair = this.quantumCrypto.getKeyPair(swarmConfig.keyId)
      if (!keyPair) {
        throw new Error('Quantum key pair not found')
      }

      const decrypted = this.quantumCrypto.decrypt(
        encryptedPayload,
        keyPair.privateKey
      )

      const message = JSON.parse(decrypted.toString('utf-8'))

      this.logEvent('message_decrypted', {
        swarmId,
        messageSize: decrypted.length,
      })

      return message
    } catch (error) {
      this.logError('message_decryption_failed', error)
      return null
    }
  }

  /**
   * Get integrated swarm status
   */
  getIntegratedSwarmStatus(swarmId: string): any {
    try {
      const metrics = this.swarmAPI.getSwarmMetrics(swarmId)
      const health = this.swarmAPI.getHealthReport(swarmId)
      const config = this.config.get(swarmId)

      const viz = this.visualizations.get(swarmId)

      return {
        swarmId,
        metrics,
        health,
        config,
        visualization: {
          active: !!viz,
          stats: viz ? {
            nodes: 0, // Would query from viz
            edges: 0,
          } : null,
        },
        security: {
          keyId: config?.keyId,
          quantum: true,
          encrypted: true,
        },
        timestamp: new Date().toISOString(),
      }
    } catch (error) {
      this.logError('status_retrieval_failed', error)
      return null
    }
  }

  /**
   * Setup event logging
   */
  private setupEventLogging(): void {
    // Log all swarm events
    const swarmIds = this.swarmAPI.listSwarms()
    swarmIds.forEach((swarmId) => {
      const swarm = this.swarmAPI.getSwarm(swarmId)
      if (swarm) {
        swarm.on('agentRegistered', (data: any) =>
          this.logEvent('agent_registered', { swarmId, ...data })
        )
        swarm.on('taskCompleted', (data: any) =>
          this.logEvent('task_completed', { swarmId, ...data })
        )
        swarm.on('consensusReached', (data: any) =>
          this.logEvent('consensus_reached', { swarmId, ...data })
        )
      }
    })
  }

  /**
   * Setup error handling
   */
  private setupErrorHandling(): void {
    // Global error handler
    process.on('uncaughtException', (error) => {
      this.logError('uncaught_exception', error)
    })

    process.on('unhandledRejection', (reason) => {
      this.logError('unhandled_rejection', reason)
    })
  }

  /**
   * Setup persistence
   */
  private setupPersistence(): void {
    // Store config periodically
    setInterval(() => {
      this.persistState()
    }, 60000) // Every minute
  }

  /**
   * Persist state
   */
  private persistState(): void {
    try {
      const state = {
        swarms: this.swarmAPI.listSwarms(),
        configs: Array.from(this.config.entries()),
        timestamp: new Date().toISOString(),
      }

      if (typeof localStorage !== 'undefined') {
        localStorage.setItem('platform_state', JSON.stringify(state))
      }

      this.logEvent('state_persisted', {})
    } catch (error) {
      this.logError('persistence_failed', error)
    }
  }

  /**
   * Restore state
   */
  restoreState(): void {
    try {
      if (typeof localStorage === 'undefined') return

      const stateStr = localStorage.getItem('platform_state')
      if (!stateStr) return

      const state = JSON.parse(stateStr)
      // Restore configs
      state.configs.forEach(([key, value]: any) => {
        this.config.set(key, value)
      })

      this.logEvent('state_restored', { swarms: state.swarms.length })
    } catch (error) {
      this.logError('restoration_failed', error)
    }
  }

  /**
   * Log event
   */
  private logEvent(type: string, data: any): void {
    const event = {
      type,
      data,
      timestamp: new Date().toISOString(),
    }

    this.eventLog.push(event)

    // Keep only last 1000 events
    if (this.eventLog.length > 1000) {
      this.eventLog.shift()
    }
  }

  /**
   * Log error
   */
  private logError(type: string, error: any): void {
    const errorEntry = {
      type,
      message: error instanceof Error ? error.message : String(error),
      timestamp: new Date().toISOString(),
    }

    this.errorLog.push(errorEntry)

    // Keep only last 100 errors
    if (this.errorLog.length > 100) {
      this.errorLog.shift()
    }

    console.error(`[${type}]`, error)
  }

  /**
   * Get event log
   */
  getEventLog(limit: number = 100): any[] {
    return this.eventLog.slice(-limit)
  }

  /**
   * Get error log
   */
  getErrorLog(limit: number = 50): any[] {
    return this.errorLog.slice(-limit)
  }

  /**
   * Get platform health
   */
  getPlatformHealth(): {
    healthy: boolean
    errors: number
    events: number
    swarms: number
    status: string
  } {
    const swarmCount = this.swarmAPI.listSwarms().length
    const errorCount = this.errorLog.length
    const healthy = errorCount < 10 && swarmCount > 0

    return {
      healthy,
      errors: errorCount,
      events: this.eventLog.length,
      swarms: swarmCount,
      status: healthy ? 'Operational' : 'Degraded',
    }
  }

  /**
   * Get comprehensive platform report
   */
  getPlatformReport(): any {
    return {
      timestamp: new Date().toISOString(),
      health: this.getPlatformHealth(),
      swarms: this.swarmAPI.listSwarms().length,
      recentEvents: this.getEventLog(20),
      recentErrors: this.getErrorLog(10),
      visualizations: this.visualizations.size,
      quantum: {
        keys: this.quantumCrypto.listKeyIds().length,
      },
    }
  }
}

/**
 * Global platform instance
 */
let globalPlatform: PlatformIntegration | null = null

/**
 * Get or create platform
 */
export function getPlatformIntegration(): PlatformIntegration {
  if (!globalPlatform) {
    globalPlatform = new PlatformIntegration()
    globalPlatform.restoreState()
  }
  return globalPlatform
}

/**
 * Initialize platform with setup
 */
export function initializePlatform(): PlatformIntegration {
  globalPlatform = new PlatformIntegration()
  globalPlatform.restoreState()
  return globalPlatform
}

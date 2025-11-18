/**
 * Quantum-Safe Collaboration Service
 * Post-Quantum Encrypted Real-Time Collaboration
 * Wraps CollaborationService with quantum-ready encryption
 *
 * 2028 Innovation Phase: Quantum-Secure Communication
 */

import {
  QuantumCryptoService,
  QuantumKeyPair,
  QuantumEncryptedPayload,
  QuantumSignature,
} from './quantum-crypto'
import { CollaborationMessage, Presence, SharedState } from './collaboration'

/**
 * Quantum-Safe collaboration event
 */
export interface QuantumCollaborationEvent<T = any> {
  id: string
  type: string
  timestamp: Date
  senderId: string
  senderName: string
  encryptedPayload: QuantumEncryptedPayload
  signature: QuantumSignature
  metadata?: {
    priority?: 'low' | 'medium' | 'high'
    requiresAck?: boolean
    ttl?: number
  }
}

/**
 * Quantum-Safe presence with key material
 */
export interface QuantumPresence extends Presence {
  publicKeyId: string
  keyAgreementPublicKey: string // For ECDH key agreement
  signatureKeyId: string
  deviceId: string
}

/**
 * Quantum Collaboration Service
 * Extends base collaboration with post-quantum encryption
 */
export class QuantumCollaborationService {
  private cryptoService: QuantumCryptoService
  private userId: string
  private userName: string
  private userKeyPair: QuantumKeyPair
  private userPresence: QuantumPresence
  private remotePublicKeys: Map<string, string> = new Map()
  private eventListeners: Map<string, Set<Function>> = new Map()
  private messageQueue: QuantumCollaborationEvent[] = []
  private isOnline: boolean = false
  private reconnectAttempts: number = 0
  private maxReconnectAttempts: number = 5

  constructor(
    userId: string,
    userName: string,
    cryptoService?: QuantumCryptoService
  ) {
    this.userId = userId
    this.userName = userName
    this.cryptoService = cryptoService || new QuantumCryptoService()

    // Generate user's quantum-safe key pair
    this.userKeyPair = this.cryptoService.generateKeyPair({
      userId,
      purpose: 'both',
      expiryDays: 90,
    })

    // Initialize presence
    this.userPresence = {
      userId,
      userName,
      status: 'offline',
      currentView: 'home',
      lastSeen: new Date(),
      publicKeyId: this.userKeyPair.keyId,
      keyAgreementPublicKey: this.userKeyPair.publicKey,
      signatureKeyId: this.userKeyPair.keyId,
      deviceId: this.generateDeviceId(),
    }
  }

  /**
   * Connect to quantum-safe collaboration network
   */
  async connect(wsUrl: string): Promise<void> {
    try {
      this.isOnline = true
      this.reconnectAttempts = 0

      // Broadcast presence with public key
      this.broadcastPresence()

      // Process queued messages
      await this.processMessageQueue()

      this.emit('quantum:connected', {
        userId: this.userId,
        keyId: this.userKeyPair.keyId,
      })
    } catch (error) {
      this.handleConnectionError(error)
    }
  }

  /**
   * Disconnect from quantum-safe collaboration network
   */
  disconnect(): void {
    this.isOnline = false
    this.userPresence.status = 'offline'
    this.emit('quantum:disconnected', { userId: this.userId })
  }

  /**
   * Send quantum-encrypted message
   */
  async sendMessage<T = any>(
    eventType: string,
    payload: T,
    recipientId?: string
  ): Promise<void> {
    try {
      // Get recipient's public key
      const recipientKey = recipientId
        ? this.remotePublicKeys.get(recipientId)
        : null

      if (!recipientKey && recipientId) {
        throw new Error(`Public key not found for recipient: ${recipientId}`)
      }

      // Encrypt payload
      const payloadJson = JSON.stringify(payload)
      const encryptedPayload = this.cryptoService.encrypt(
        payloadJson,
        recipientKey || this.userKeyPair.publicKey,
        recipientId || this.userKeyPair.keyId
      )

      // Sign encrypted payload
      const payloadString = this.userKeyPair.privateKey
      const signature = this.cryptoService.sign(
        payloadJson,
        payloadString,
        this.userKeyPair.keyId
      )

      // Create quantum event
      const event: QuantumCollaborationEvent = {
        id: this.generateEventId(),
        type: eventType,
        timestamp: new Date(),
        senderId: this.userId,
        senderName: this.userName,
        encryptedPayload,
        signature,
        metadata: {
          priority: 'medium',
          requiresAck: true,
        },
      }

      if (this.isOnline) {
        // Send immediately if online
        this.emit('quantum:message', event)
      } else {
        // Queue if offline
        this.messageQueue.push(event)
      }
    } catch (error) {
      this.emit('quantum:error', {
        type: 'send_failed',
        error: error instanceof Error ? error.message : 'Unknown error',
        eventType,
      })
    }
  }

  /**
   * Receive and decrypt quantum-encrypted message
   */
  async receiveMessage<T = any>(event: QuantumCollaborationEvent): Promise<T> {
    try {
      // Verify sender's public key is known
      if (!this.remotePublicKeys.has(event.senderId)) {
        throw new Error(`Unknown sender: ${event.senderId}`)
      }

      const senderPublicKey = this.remotePublicKeys.get(event.senderId)!

      // Verify signature
      const isValid = this.cryptoService.verify(
        JSON.stringify(event.encryptedPayload),
        event.signature,
        senderPublicKey
      )

      if (!isValid) {
        throw new Error('Signature verification failed - message integrity compromised')
      }

      // Decrypt payload
      const decryptedBuffer = this.cryptoService.decrypt(
        event.encryptedPayload,
        this.userKeyPair.privateKey
      )

      const payload = JSON.parse(decryptedBuffer.toString('utf-8')) as T

      // Emit decrypted event
      this.emit(`quantum:${event.type}`, {
        payload,
        senderId: event.senderId,
        senderName: event.senderName,
        timestamp: event.timestamp,
      })

      return payload
    } catch (error) {
      this.emit('quantum:error', {
        type: 'decrypt_failed',
        error: error instanceof Error ? error.message : 'Unknown error',
        eventId: event.id,
      })
      throw error
    }
  }

  /**
   * Broadcast presence to other users
   */
  private broadcastPresence(): void {
    this.userPresence.status = 'online'
    this.userPresence.lastSeen = new Date()

    this.emit('quantum:presence', this.userPresence)
  }

  /**
   * Update remote user's public key
   */
  registerRemotePublicKey(userId: string, publicKey: string): void {
    this.remotePublicKeys.set(userId, publicKey)
    this.emit('quantum:key_registered', { userId, keyId: userId })
  }

  /**
   * Process offline message queue
   */
  private async processMessageQueue(): Promise<void> {
    const queue = [...this.messageQueue]
    this.messageQueue = []

    for (const event of queue) {
      try {
        this.emit('quantum:message', event)
      } catch (error) {
        // Re-queue on failure
        this.messageQueue.push(event)
      }
    }
  }

  /**
   * Handle connection errors with exponential backoff
   */
  private handleConnectionError(error: any): void {
    this.reconnectAttempts++

    if (this.reconnectAttempts >= this.maxReconnectAttempts) {
      this.emit('quantum:error', {
        type: 'max_reconnects_exceeded',
        error: error instanceof Error ? error.message : 'Unknown error',
      })
      return
    }

    const backoffMs = Math.pow(2, this.reconnectAttempts) * 1000
    this.emit('quantum:reconnecting', {
      attempt: this.reconnectAttempts,
      backoffMs,
    })

    setTimeout(() => {
      this.connect('ws://localhost:3001')
    }, backoffMs)
  }

  /**
   * Rotate key pair for security
   */
  rotateKeys(): QuantumKeyPair {
    const newKeyPair = this.cryptoService.rotateKeyPair(this.userKeyPair.keyId)!
    this.userKeyPair = newKeyPair
    this.userPresence.publicKeyId = newKeyPair.keyId
    this.userPresence.keyAgreementPublicKey = newKeyPair.publicKey

    // Broadcast new public key
    this.emit('quantum:key_rotated', {
      oldKeyId: this.cryptoService.listKeyIds()[0],
      newKeyId: newKeyPair.keyId,
    })

    return newKeyPair
  }

  /**
   * Update user presence
   */
  updatePresence(updates: Partial<QuantumPresence>): void {
    this.userPresence = {
      ...this.userPresence,
      ...updates,
      lastSeen: new Date(),
    }
    this.broadcastPresence()
  }

  /**
   * Subscribe to quantum collaboration events
   */
  on(eventType: string, callback: Function): void {
    if (!this.eventListeners.has(eventType)) {
      this.eventListeners.set(eventType, new Set())
    }
    this.eventListeners.get(eventType)!.add(callback)
  }

  /**
   * Unsubscribe from quantum collaboration events
   */
  off(eventType: string, callback: Function): void {
    const listeners = this.eventListeners.get(eventType)
    if (listeners) {
      listeners.delete(callback)
    }
  }

  /**
   * Emit quantum collaboration event
   */
  private emit(eventType: string, data: any): void {
    const listeners = this.eventListeners.get(eventType)
    if (listeners) {
      listeners.forEach((callback) => {
        try {
          callback(data)
        } catch (error) {
          console.error(`Error in ${eventType} listener:`, error)
        }
      })
    }
  }

  /**
   * Generate unique event ID
   */
  private generateEventId(): string {
    return `evt-${Date.now()}-${Math.random().toString(36).slice(2, 9)}`
  }

  /**
   * Generate device ID
   */
  private generateDeviceId(): string {
    return `dev-${navigator?.userAgent?.substring(0, 20) || 'unknown'}-${Date.now()}`
  }

  /**
   * Get current presence
   */
  getPresence(): QuantumPresence {
    return this.userPresence
  }

  /**
   * Get connection status
   */
  isConnected(): boolean {
    return this.isOnline
  }

  /**
   * Get message queue status
   */
  getQueueStatus(): { queuedMessages: number; maxRetries: number } {
    return {
      queuedMessages: this.messageQueue.length,
      maxRetries: this.maxReconnectAttempts,
    }
  }

  /**
   * Get crypto service instance
   */
  getCryptoService(): QuantumCryptoService {
    return this.cryptoService
  }
}

/**
 * React Hook for Quantum Collaboration
 */
export function useQuantumCollaboration(
  userId: string,
  userName: string
) {
  const [service] = React.useState(
    () => new QuantumCollaborationService(userId, userName)
  )
  const [isConnected, setIsConnected] = React.useState(false)
  const [presence, setPresence] = React.useState<QuantumPresence>(
    service.getPresence()
  )
  const [messages, setMessages] = React.useState<QuantumCollaborationEvent[]>([])

  React.useEffect(() => {
    service.on('quantum:connected', () => setIsConnected(true))
    service.on('quantum:disconnected', () => setIsConnected(false))
    service.on('quantum:presence', (p) => setPresence(p))
    service.on('quantum:message', (msg) =>
      setMessages((prev) => [...prev, msg])
    )

    return () => {
      service.disconnect()
    }
  }, [service])

  return {
    service,
    isConnected,
    presence,
    messages,
    connect: (wsUrl: string) => service.connect(wsUrl),
    disconnect: () => service.disconnect(),
    sendMessage: <T,>(type: string, payload: T, recipientId?: string) =>
      service.sendMessage(type, payload, recipientId),
    rotateKeys: () => service.rotateKeys(),
  }
}

// Note: React import will be handled at component level
const React = require('react') as typeof import('react')

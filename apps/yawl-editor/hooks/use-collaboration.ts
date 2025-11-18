import { useEffect, useState, useCallback, useRef } from 'react'
import { CollaborationService, type Presence, type CollaborationMessage } from '@/lib/collaboration'

interface UseCollaborationOptions {
  wsUrl: string
  userId: string
  userName: string
  autoConnect?: boolean
  onConnected?: () => void
  onDisconnected?: () => void
  onError?: (error: Error) => void
}

/**
 * Hook for real-time collaboration with presence tracking and event broadcasting
 */
export function useCollaboration(options: UseCollaborationOptions) {
  const [isConnected, setIsConnected] = useState(false)
  const [activeUsers, setActiveUsers] = useState<Presence[]>([])
  const [recentActivity, setRecentActivity] = useState<CollaborationMessage[]>([])
  const [error, setError] = useState<Error | null>(null)

  const serviceRef = useRef<CollaborationService | null>(null)
  const activityRef = useRef<CollaborationMessage[]>([])

  // Initialize collaboration service
  useEffect(() => {
    if (serviceRef.current) return

    const service = new CollaborationService(
      options.wsUrl,
      options.userId,
      options.userName
    )

    serviceRef.current = service

    // Subscribe to all event types
    const eventTypes = [
      'case:created',
      'case:updated',
      'case:deleted',
      'workitem:created',
      'workitem:updated',
      'workitem:allocated',
      'process:created',
      'process:updated',
      'user:online',
      'user:offline',
    ] as const

    eventTypes.forEach((eventType) => {
      service.on(eventType, (message) => {
        // Update recent activity (keep last 20 items)
        activityRef.current = [message, ...activityRef.current].slice(0, 20)
        setRecentActivity([...activityRef.current])

        // Update active users
        setActiveUsers(service.getActiveUsers())
      })
    })

    // Auto-connect if enabled
    if (options.autoConnect !== false) {
      service
        .connect()
        .then(() => {
          setIsConnected(true)
          setActiveUsers(service.getWorkspaceMembers())
          options.onConnected?.()
        })
        .catch((err) => {
          setError(err)
          options.onError?.(err)
        })
    }

    return () => {
      if (options.autoConnect !== false) {
        service.disconnect()
      }
    }
  }, [options])

  // Connection control
  const connect = useCallback(async () => {
    if (!serviceRef.current) return
    try {
      await serviceRef.current.connect()
      setIsConnected(true)
      setActiveUsers(serviceRef.current.getWorkspaceMembers())
      setError(null)
      options.onConnected?.()
    } catch (err) {
      const error = err instanceof Error ? err : new Error('Connection failed')
      setError(error)
      options.onError?.(error)
      throw error
    }
  }, [options])

  const disconnect = useCallback(() => {
    if (!serviceRef.current) return
    serviceRef.current.disconnect()
    setIsConnected(false)
    options.onDisconnected?.()
  }, [options])

  // Event management
  const subscribe = useCallback(
    (eventType: Parameters<CollaborationService['on']>[0], callback: (message: CollaborationMessage) => void) => {
      if (!serviceRef.current) return () => {}
      serviceRef.current.on(eventType, callback)
      return () => serviceRef.current?.off(eventType, callback)
    },
    []
  )

  const broadcast = useCallback(
    (eventType: Parameters<CollaborationService['broadcast']>[0], data: unknown, resourceId: string) => {
      if (!serviceRef.current) return
      serviceRef.current.broadcast(eventType, data, resourceId)
    },
    []
  )

  // View context
  const updateView = useCallback((view: string) => {
    if (!serviceRef.current) return
    serviceRef.current.updateView(view)
  }, [])

  // Get current user presence
  const getCurrentUserPresence = useCallback(() => {
    if (!serviceRef.current) return null
    return serviceRef.current.getPresence()
  }, [])

  // Get all active users
  const getActiveUsersList = useCallback(() => {
    if (!serviceRef.current) return []
    return serviceRef.current.getActiveUsers()
  }, [])

  // Get all workspace members
  const getAllMembers = useCallback(() => {
    if (!serviceRef.current) return []
    return serviceRef.current.getWorkspaceMembers()
  }, [])

  return {
    isConnected,
    activeUsers,
    recentActivity,
    error,
    connect,
    disconnect,
    subscribe,
    broadcast,
    updateView,
    getCurrentUserPresence,
    getActiveUsersList,
    getAllMembers,
  }
}

/**
 * Hook for subscribing to specific collaboration events
 */
export function useCollaborationEvent(
  collaboration: ReturnType<typeof useCollaboration>,
  eventType: Parameters<CollaborationService['on']>[0],
  callback: (message: CollaborationMessage) => void
) {
  useEffect(() => {
    const unsubscribe = collaboration.subscribe(eventType, callback)
    return unsubscribe
  }, [collaboration, eventType, callback])
}

/**
 * Hook for broadcasting events
 */
export function useCollaborationBroadcast(
  collaboration: ReturnType<typeof useCollaboration>
) {
  return useCallback(
    (eventType: Parameters<CollaborationService['broadcast']>[0], data: unknown, resourceId: string) => {
      collaboration.broadcast(eventType, data, resourceId)
    },
    [collaboration]
  )
}

import { useState, useCallback } from "react"

export interface HealthCheck {
  id: string
  name: string
  status: "healthy" | "degraded" | "unhealthy"
  lastCheck: Date
  responseTime: number
  checks: CheckResult[]
}

export interface CheckResult {
  name: string
  status: "pass" | "warn" | "fail"
  message: string
  value?: any
}

export interface CircuitBreaker {
  id: string
  name: string
  state: "closed" | "open" | "half-open"
  failureCount: number
  successCount: number
  lastFailure?: Date
  threshold: {
    failureThreshold: number
    successThreshold: number
    timeout: number
  }
}

export interface RetryPolicy {
  maxAttempts: number
  initialDelay: number
  maxDelay: number
  backoffMultiplier: number
  jitter: boolean
}

export interface AutoHealingAction {
  id: string
  type: "restart" | "scale" | "failover" | "compensate" | "alert"
  condition: string
  action: string
  enabled: boolean
  lastExecuted?: Date
  successCount: number
  failureCount: number
}

export function useAutoHealing() {
  const [healthChecks, setHealthChecks] = useState<Map<string, HealthCheck>>(new Map())
  const [circuitBreakers, setCircuitBreakers] = useState<Map<string, CircuitBreaker>>(new Map())
  const [healingActions, setHealingActions] = useState<AutoHealingAction[]>([])
  const [healingLog, setHealingLog] = useState<Array<{
    timestamp: Date
    action: string
    status: "success" | "failure"
    reason?: string
  }>>([])

  // Register health check
  const registerHealthCheck = useCallback(
    (name: string, checks: () => Promise<CheckResult[]>) => {
      const checkId = `health-${Date.now()}`

      const performCheck = async () => {
        try {
          const startTime = Date.now()
          const results = await checks()
          const responseTime = Date.now() - startTime

          // Determine overall status
          let status: "healthy" | "degraded" | "unhealthy" = "healthy"
          if (results.some((r) => r.status === "fail")) status = "unhealthy"
          else if (results.some((r) => r.status === "warn")) status = "degraded"

          const healthCheck: HealthCheck = {
            id: checkId,
            name,
            status,
            lastCheck: new Date(),
            responseTime,
            checks: results,
          }

          setHealthChecks((prev) => new Map(prev).set(checkId, healthCheck))

          // Trigger auto-healing if unhealthy
          if (status === "unhealthy") {
            executeHealingActions(name)
          }

          return healthCheck
        } catch (error) {
          const healthCheck: HealthCheck = {
            id: checkId,
            name,
            status: "unhealthy",
            lastCheck: new Date(),
            responseTime: -1,
            checks: [
              {
                name: "Error",
                status: "fail",
                message: error instanceof Error ? error.message : "Unknown error",
              },
            ],
          }

          setHealthChecks((prev) => new Map(prev).set(checkId, healthCheck))
          executeHealingActions(name)
          return healthCheck
        }
      }

      // Run check immediately and then periodically
      performCheck()
      const interval = setInterval(performCheck, 30000) // Every 30 seconds

      return () => clearInterval(interval)
    },
    []
  )

  // Create circuit breaker
  const createCircuitBreaker = useCallback(
    (
      name: string,
      options: {
        failureThreshold?: number
        successThreshold?: number
        timeout?: number
      } = {}
    ): CircuitBreaker => {
      const breaker: CircuitBreaker = {
        id: `cb-${Date.now()}`,
        name,
        state: "closed",
        failureCount: 0,
        successCount: 0,
        threshold: {
          failureThreshold: options.failureThreshold || 5,
          successThreshold: options.successThreshold || 3,
          timeout: options.timeout || 30000,
        },
      }

      setCircuitBreakers((prev) => new Map(prev).set(breaker.id, breaker))
      return breaker
    },
    []
  )

  // Record circuit breaker event
  const recordCircuitBreakerEvent = useCallback(
    (breakerId: string, success: boolean) => {
      setCircuitBreakers((prev) => {
        const breaker = prev.get(breakerId)
        if (!breaker) return prev

        const updated = { ...breaker }

        if (success) {
          updated.successCount++
          updated.failureCount = 0

          // Transition from half-open to closed
          if (updated.state === "half-open" && updated.successCount >= updated.threshold.successThreshold) {
            updated.state = "closed"
            updated.successCount = 0
          }
        } else {
          updated.failureCount++
          updated.successCount = 0
          updated.lastFailure = new Date()

          // Transition from closed to open
          if (updated.state === "closed" && updated.failureCount >= updated.threshold.failureThreshold) {
            updated.state = "open"

            // Auto-transition to half-open after timeout
            setTimeout(() => {
              setCircuitBreakers((prev2) => {
                const breaker2 = prev2.get(breakerId)
                if (breaker2 && breaker2.state === "open") {
                  return new Map(prev2).set(breakerId, {
                    ...breaker2,
                    state: "half-open",
                  })
                }
                return prev2
              })
            }, updated.threshold.timeout)
          }
        }

        return new Map(prev).set(breakerId, updated)
      })
    },
    []
  )

  // Get retry policy
  const getRetryPolicy = useCallback((type: "fast" | "standard" | "slow"): RetryPolicy => {
    const policies = {
      fast: {
        maxAttempts: 3,
        initialDelay: 100,
        maxDelay: 1000,
        backoffMultiplier: 2,
        jitter: true,
      },
      standard: {
        maxAttempts: 5,
        initialDelay: 1000,
        maxDelay: 30000,
        backoffMultiplier: 2,
        jitter: true,
      },
      slow: {
        maxAttempts: 10,
        initialDelay: 5000,
        maxDelay: 300000,
        backoffMultiplier: 1.5,
        jitter: true,
      },
    }

    return policies[type] || policies.standard
  }, [])

  // Execute with retry
  const executeWithRetry = useCallback(
    async <T,>(
      operation: () => Promise<T>,
      policy: RetryPolicy = getRetryPolicy("standard"),
      onRetry?: (attempt: number, error: Error) => void
    ): Promise<T> => {
      let lastError: Error | null = null

      for (let attempt = 1; attempt <= policy.maxAttempts; attempt++) {
        try {
          return await operation()
        } catch (error) {
          lastError = error instanceof Error ? error : new Error(String(error))

          if (attempt === policy.maxAttempts) {
            throw lastError
          }

          onRetry?.(attempt, lastError)

          // Calculate delay
          let delay = Math.min(
            policy.initialDelay * Math.pow(policy.backoffMultiplier, attempt - 1),
            policy.maxDelay
          )

          // Add jitter
          if (policy.jitter) {
            delay += Math.random() * delay * 0.1
          }

          await new Promise((resolve) => setTimeout(resolve, delay))
        }
      }

      throw lastError || new Error("Max retry attempts reached")
    },
    [getRetryPolicy]
  )

  // Define healing action
  const defineHealingAction = useCallback(
    (
      type: "restart" | "scale" | "failover" | "compensate" | "alert",
      condition: string,
      action: string
    ): AutoHealingAction => {
      const healingAction: AutoHealingAction = {
        id: `action-${Date.now()}`,
        type,
        condition,
        action,
        enabled: true,
        successCount: 0,
        failureCount: 0,
      }

      setHealingActions((prev) => [...prev, healingAction])
      return healingAction
    },
    []
  )

  // Execute healing actions
  const executeHealingActions = useCallback(
    (resourceName: string) => {
      const applicableActions = healingActions.filter(
        (action) => action.enabled && action.condition.includes(resourceName)
      )

      applicableActions.forEach((action) => {
        ;(async () => {
          try {
            // Mock execution of healing action
            console.log(`Executing healing action: ${action.action}`)

            // Simulate healing action
            await new Promise((resolve) => setTimeout(resolve, 1000))

            // Log success
            setHealingLog((prev) => [
              ...prev,
              {
                timestamp: new Date(),
                action: action.action,
                status: "success",
              },
            ])

            // Update action stats
            setHealingActions((prev) =>
              prev.map((a) => (a.id === action.id ? { ...a, successCount: a.successCount + 1 } : a))
            )
          } catch (error) {
            // Log failure
            setHealingLog((prev) => [
              ...prev,
              {
                timestamp: new Date(),
                action: action.action,
                status: "failure",
                reason: error instanceof Error ? error.message : "Unknown error",
              },
            ])

            // Update action stats
            setHealingActions((prev) =>
              prev.map((a) => (a.id === action.id ? { ...a, failureCount: a.failureCount + 1 } : a))
            )
          }
        })()
      })
    },
    [healingActions]
  )

  // Get healing status
  const getHealingStatus = useCallback(() => {
    const recentLogs = healingLog.slice(0, 100)
    const successCount = recentLogs.filter((l) => l.status === "success").length
    const failureCount = recentLogs.filter((l) => l.status === "failure").length

    return {
      totalActions: recentLogs.length,
      successCount,
      failureCount,
      successRate: totalActions > 0 ? successCount / recentLogs.length : 0,
      enabledActions: healingActions.filter((a) => a.enabled).length,
      totalActions: healingActions.length,
    }
  }, [healingActions, healingLog])

  return {
    // Health checks
    registerHealthCheck,
    healthChecks: Object.fromEntries(healthChecks),

    // Circuit breaker
    createCircuitBreaker,
    recordCircuitBreakerEvent,
    circuitBreakers: Object.fromEntries(circuitBreakers),

    // Retry logic
    getRetryPolicy,
    executeWithRetry,

    // Healing actions
    defineHealingAction,
    healingActions,
    executeHealingActions,

    // Status
    healingLog,
    getHealingStatus,
  }
}

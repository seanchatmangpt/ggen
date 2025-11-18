import { useState, useCallback, useRef } from "react"

export interface Span {
  id: string
  traceId: string
  parentSpanId?: string
  name: string
  service: string
  startTime: number
  endTime?: number
  duration?: number
  status: "active" | "success" | "error"
  attributes: Record<string, any>
  events: SpanEvent[]
  links: SpanLink[]
  samplingRate: number
}

export interface SpanEvent {
  name: string
  timestamp: number
  attributes?: Record<string, any>
}

export interface SpanLink {
  traceId: string
  spanId: string
  attributes?: Record<string, any>
}

export interface Trace {
  traceId: string
  spans: Span[]
  startTime: number
  endTime?: number
  duration?: number
  rootSpan: Span
  status: "active" | "success" | "error"
  attributes: Record<string, any>
}

export interface Metric {
  name: string
  value: number
  unit?: string
  timestamp: number
  attributes?: Record<string, any>
  type: "counter" | "gauge" | "histogram"
}

export interface Log {
  timestamp: number
  level: "debug" | "info" | "warn" | "error"
  message: string
  traceId?: string
  spanId?: string
  attributes?: Record<string, any>
}

export function useObservability(serviceName: string) {
  const [traces, setTraces] = useState<Map<string, Trace>>(new Map())
  const [metrics, setMetrics] = useState<Metric[]>([])
  const [logs, setLogs] = useState<Log[]>([])
  const [activeSpans, setActiveSpans] = useState<Map<string, Span>>(new Map())

  const traceIdRef = useRef<string>("")
  const spanStackRef = useRef<Span[]>([])

  // Generate trace ID
  const generateTraceId = useCallback((): string => {
    return `trace-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`
  }, [])

  // Start a new trace
  const startTrace = useCallback(
    (name: string, attributes: Record<string, any> = {}): string => {
      const traceId = generateTraceId()
      traceIdRef.current = traceId

      const rootSpan: Span = {
        id: `span-${Date.now()}-root`,
        traceId,
        name,
        service: serviceName,
        startTime: Date.now(),
        status: "active",
        attributes: {
          ...attributes,
          "service.name": serviceName,
        },
        events: [],
        links: [],
        samplingRate: 1.0,
      }

      spanStackRef.current = [rootSpan]
      setActiveSpans((prev) => new Map(prev).set(rootSpan.id, rootSpan))

      return traceId
    },
    [serviceName, generateTraceId]
  )

  // Start a span
  const startSpan = useCallback(
    (name: string, attributes: Record<string, any> = {}): string => {
      const parentSpan = spanStackRef.current[spanStackRef.current.length - 1]
      if (!parentSpan) {
        // Auto-create root span if none exists
        startTrace(name)
        return startSpan(name, attributes)
      }

      const span: Span = {
        id: `span-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`,
        traceId: parentSpan.traceId,
        parentSpanId: parentSpan.id,
        name,
        service: serviceName,
        startTime: Date.now(),
        status: "active",
        attributes: {
          ...attributes,
          "service.name": serviceName,
        },
        events: [],
        links: [],
        samplingRate: 1.0,
      }

      spanStackRef.current.push(span)
      setActiveSpans((prev) => new Map(prev).set(span.id, span))

      return span.id
    },
    [serviceName, startTrace]
  )

  // End a span
  const endSpan = useCallback(
    (spanId: string, status: "success" | "error" = "success", errorMessage?: string) => {
      const span = spanStackRef.current.find((s) => s.id === spanId)
      if (!span) return

      const endTime = Date.now()
      const updatedSpan: Span = {
        ...span,
        endTime,
        duration: endTime - span.startTime,
        status,
      }

      if (errorMessage && status === "error") {
        updatedSpan.events.push({
          name: "error",
          timestamp: endTime,
          attributes: { message: errorMessage },
        })
      }

      // Update active spans
      setActiveSpans((prev) => {
        const newMap = new Map(prev)
        newMap.set(spanId, updatedSpan)
        return newMap
      })

      // Remove from stack
      spanStackRef.current = spanStackRef.current.filter((s) => s.id !== spanId)

      // If this was the root span, finalize the trace
      if (!spanStackRef.current.length && updatedSpan.traceId) {
        const allSpans = Array.from(
          new Map([
            ...Array.from(activeSpans.entries()),
            [spanId, updatedSpan],
          ]).values()
        )

        const trace: Trace = {
          traceId: updatedSpan.traceId,
          spans: allSpans,
          startTime: updatedSpan.startTime,
          endTime,
          duration: endTime - updatedSpan.startTime,
          rootSpan: updatedSpan,
          status,
          attributes: updatedSpan.attributes,
        }

        setTraces((prev) => new Map(prev).set(trace.traceId, trace))
      }
    },
    [activeSpans]
  )

  // Add event to current span
  const addEvent = useCallback(
    (name: string, attributes?: Record<string, any>) => {
      const currentSpan = spanStackRef.current[spanStackRef.current.length - 1]
      if (!currentSpan) return

      currentSpan.events.push({
        name,
        timestamp: Date.now(),
        attributes,
      })
    },
    []
  )

  // Record metric
  const recordMetric = useCallback(
    (
      name: string,
      value: number,
      type: "counter" | "gauge" | "histogram" = "gauge",
      attributes?: Record<string, any>
    ) => {
      const currentSpan = spanStackRef.current[spanStackRef.current.length - 1]

      const metric: Metric = {
        name,
        value,
        type,
        timestamp: Date.now(),
        attributes: {
          ...attributes,
          traceId: currentSpan?.traceId,
          spanId: currentSpan?.id,
        },
      }

      setMetrics((prev) => [...prev, metric])
    },
    []
  )

  // Log message
  const log = useCallback(
    (
      level: "debug" | "info" | "warn" | "error",
      message: string,
      attributes?: Record<string, any>
    ) => {
      const currentSpan = spanStackRef.current[spanStackRef.current.length - 1]

      const logEntry: Log = {
        timestamp: Date.now(),
        level,
        message,
        traceId: currentSpan?.traceId,
        spanId: currentSpan?.id,
        attributes,
      }

      setLogs((prev) => [...prev, logEntry])

      // Also add as event to current span
      if (currentSpan) {
        addEvent(`log.${level}`, { message, ...attributes })
      }
    },
    [addEvent]
  )

  // Get trace
  const getTrace = useCallback(
    (traceId: string): Trace | undefined => {
      return traces.get(traceId)
    },
    [traces]
  )

  // Get metrics
  const getMetrics = useCallback(
    (filters?: {
      name?: string
      type?: string
      startTime?: number
      endTime?: number
    }): Metric[] => {
      return metrics.filter((m) => {
        if (filters?.name && m.name !== filters.name) return false
        if (filters?.type && m.type !== filters.type) return false
        if (filters?.startTime && m.timestamp < filters.startTime) return false
        if (filters?.endTime && m.timestamp > filters.endTime) return false
        return true
      })
    },
    [metrics]
  )

  // Get logs
  const getLogs = useCallback(
    (filters?: {
      level?: string
      traceId?: string
      startTime?: number
      endTime?: number
    }): Log[] => {
      return logs.filter((l) => {
        if (filters?.level && l.level !== filters.level) return false
        if (filters?.traceId && l.traceId !== filters.traceId) return false
        if (filters?.startTime && l.timestamp < filters.startTime) return false
        if (filters?.endTime && l.timestamp > filters.endTime) return false
        return true
      })
    },
    [logs]
  )

  // Get statistics
  const getStatistics = useCallback(
    (hours: number = 1) => {
      const cutoff = Date.now() - hours * 60 * 60 * 1000
      const recentTraces = Array.from(traces.values()).filter((t) => t.startTime > cutoff)
      const recentMetrics = metrics.filter((m) => m.timestamp > cutoff)
      const recentLogs = logs.filter((l) => l.timestamp > cutoff)

      return {
        traces: {
          total: recentTraces.length,
          successful: recentTraces.filter((t) => t.status === "success").length,
          failed: recentTraces.filter((t) => t.status === "error").length,
          avgDuration: recentTraces.reduce((sum, t) => sum + (t.duration || 0), 0) / recentTraces.length,
        },
        metrics: {
          total: recentMetrics.length,
          byType: Object.fromEntries(
            new Map(
              recentMetrics.reduce((acc: [string, number][], m) => {
                const key = m.type
                const existing = acc.find(([k]) => k === key)
                if (existing) {
                  existing[1]++
                } else {
                  acc.push([key, 1])
                }
                return acc
              }, [])
            )
          ),
        },
        logs: {
          total: recentLogs.length,
          byLevel: Object.fromEntries(
            new Map(
              recentLogs.reduce((acc: [string, number][], l) => {
                const key = l.level
                const existing = acc.find(([k]) => k === key)
                if (existing) {
                  existing[1]++
                } else {
                  acc.push([key, 1])
                }
                return acc
              }, [])
            )
          ),
        },
      }
    },
    [traces, metrics, logs]
  )

  return {
    // Tracing
    startTrace,
    startSpan,
    endSpan,
    addEvent,
    getTrace,
    traces: Object.fromEntries(traces),

    // Metrics
    recordMetric,
    getMetrics,

    // Logging
    log,
    getLogs,

    // Statistics
    getStatistics,

    // State
    activeSpans: Object.fromEntries(activeSpans),
  }
}

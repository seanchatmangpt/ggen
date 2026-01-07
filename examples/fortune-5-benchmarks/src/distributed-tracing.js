/**
 * DISTRIBUTED TRACING - W3C TRACE CONTEXT STANDARD
 * ============================================================================
 * Correlate observations across service boundaries
 * Every benchmark run gets a unique trace for debugging and analysis
 *
 * Standard: https://www.w3.org/TR/trace-context/
 * Format: traceparent: 00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01
 *         tracestate: vendorname=opaquevalue
 * ============================================================================
 */

/**
 * Globally unique span ID generator
 */
function generateSpanId() {
  return Math.random().toString(16).substring(2, 18).padStart(16, '0');
}

/**
 * Trace ID generator (128-bit, random)
 */
function generateTraceId() {
  return (
    Math.random().toString(16).substring(2, 18).padStart(16, '0') +
    Math.random().toString(16).substring(2, 18).padStart(16, '0')
  );
}

/**
 * W3C Trace Context implementation
 */
export class TraceContext {
  constructor(traceId = null, spanId = null, traceFlags = 0x01) {
    this.traceId = traceId || generateTraceId();
    this.spanId = spanId || generateSpanId();
    this.traceFlags = traceFlags; // sampled (0x01)
    this.traceState = new Map();
    this.parentSpanId = null;
  }

  /**
   * Parse W3C traceparent header
   */
  static fromHeader(traceparentHeader) {
    const parts = traceparentHeader.split('-');
    if (parts.length < 4) {
      throw new Error('Invalid traceparent header format');
    }

    const [version, traceId, spanId, traceFlags] = parts;

    if (version !== '00') {
      throw new Error(`Unsupported trace context version: ${version}`);
    }

    const context = new TraceContext(
      traceId,
      spanId,
      parseInt(traceFlags, 16)
    );

    return context;
  }

  /**
   * Generate W3C traceparent header
   */
  toHeader() {
    return `00-${this.traceId}-${this.spanId}-${this.traceFlags.toString(16).padStart(2, '0')}`;
  }

  /**
   * Create child span (for distributed operations)
   */
  createChildSpan() {
    const child = new TraceContext(this.traceId, generateSpanId(), this.traceFlags);
    child.parentSpanId = this.spanId;
    return child;
  }

  /**
   * Add vendor-specific trace state
   */
  addTraceState(vendor, value) {
    this.traceState.set(vendor, value);
  }

  /**
   * Get trace state header
   */
  getTraceStateHeader() {
    const pairs = Array.from(this.traceState.entries())
      .map(([k, v]) => `${k}=${v}`)
      .join(',');
    return pairs || null;
  }

  /**
   * Check if trace is sampled
   */
  isSampled() {
    return (this.traceFlags & 0x01) !== 0;
  }

  /**
   * To JSON for logging
   */
  toJSON() {
    return {
      traceId: this.traceId,
      spanId: this.spanId,
      parentSpanId: this.parentSpanId,
      traceFlags: this.traceFlags,
      traceparent: this.toHeader(),
      tracestate: this.getTraceStateHeader(),
    };
  }
}

/**
 * Structured logging with trace context
 */
export class TracedLog {
  constructor(traceContext) {
    this.trace = traceContext;
    this.logs = [];
  }

  /**
   * Log with automatic trace injection
   */
  log(level, message, data = {}) {
    const entry = {
      timestamp: new Date().toISOString(),
      level,
      message,
      trace: {
        traceId: this.trace.traceId,
        spanId: this.trace.spanId,
        parentSpanId: this.trace.parentSpanId,
      },
      ...data,
    };

    this.logs.push(entry);

    // Also print for visibility
    const prefix = `[${this.trace.traceId.substring(0, 8)}] `;
    console.log(`${prefix}${level}: ${message}`);

    return entry;
  }

  info(message, data) {
    return this.log('INFO', message, data);
  }

  error(message, data) {
    return this.log('ERROR', message, data);
  }

  warn(message, data) {
    return this.log('WARN', message, data);
  }

  debug(message, data) {
    return this.log('DEBUG', message, data);
  }

  /**
   * Export logs as JSON for analysis
   */
  toJSON() {
    return {
      traceId: this.trace.traceId,
      entries: this.logs,
    };
  }
}

/**
 * Span metric recording with automatic tracing
 */
export class SpanMetrics {
  constructor(traceContext) {
    this.trace = traceContext;
    this.startTime = Date.now();
    this.endTime = null;
    this.metrics = {};
  }

  /**
   * Record a metric associated with this span
   */
  recordMetric(name, value, unit) {
    this.metrics[name] = { value, unit, timestamp: Date.now() };
  }

  /**
   * Mark span as complete
   */
  finish() {
    this.endTime = Date.now();
  }

  /**
   * Get span duration
   */
  getDuration() {
    return (this.endTime || Date.now()) - this.startTime;
  }

  /**
   * Export span data
   */
  toJSON() {
    return {
      traceId: this.trace.traceId,
      spanId: this.trace.spanId,
      parentSpanId: this.trace.parentSpanId,
      startTime: this.startTime,
      endTime: this.endTime,
      duration: this.getDuration(),
      metrics: this.metrics,
    };
  }
}

/**
 * Trace collector for analysis
 */
export class TraceCollector {
  constructor() {
    this.traces = [];
    this.spans = [];
  }

  /**
   * Start a new trace
   */
  startTrace() {
    const trace = new TraceContext();
    this.traces.push(trace);
    return trace;
  }

  /**
   * Record a span
   */
  recordSpan(span) {
    this.spans.push(span);
  }

  /**
   * Get traces by property
   */
  query(predicate) {
    return this.spans.filter(predicate);
  }

  /**
   * Analyze latency distribution
   */
  analyzeLatency() {
    const durations = this.spans.map(s => s.getDuration());
    durations.sort((a, b) => a - b);

    return {
      count: durations.length,
      min: durations[0],
      max: durations[durations.length - 1],
      mean: durations.reduce((a, b) => a + b) / durations.length,
      p50: durations[Math.floor(durations.length * 0.5)],
      p95: durations[Math.floor(durations.length * 0.95)],
      p99: durations[Math.floor(durations.length * 0.99)],
    };
  }

  /**
   * Export all traces for external analysis
   */
  export() {
    return {
      traceCount: this.traces.length,
      spanCount: this.spans.length,
      traces: this.traces.map(t => t.toJSON()),
      spans: this.spans.map(s => s.toJSON()),
    };
  }
}

/**
 * Fortune 5 integration
 */
export class Fortune5Tracing {
  constructor() {
    this.collector = new TraceCollector();
    this.currentTrace = null;
  }

  /**
   * Start Fortune 5 benchmark trace
   */
  startBenchmark(patternName) {
    this.currentTrace = this.collector.startTrace();
    const logger = new TracedLog(this.currentTrace);

    logger.info(`Starting benchmark: ${patternName}`, {
      pattern: patternName,
    });

    return {
      trace: this.currentTrace,
      logger,
      span: null,
    };
  }

  /**
   * Record pattern measurement
   */
  recordPattern(patternName, measurement) {
    const trace = this.currentTrace;
    if (!trace) return;

    const span = new SpanMetrics(trace);
    span.recordMetric(patternName, measurement.value, measurement.unit);
    span.finish();

    this.collector.recordSpan(span);

    return span;
  }

  /**
   * Get trace analysis
   */
  analyze() {
    return {
      totalTraces: this.collector.traces.length,
      totalSpans: this.collector.spans.length,
      latencyStats: this.collector.analyzeLatency(),
    };
  }

  /**
   * Export for external tools (Jaeger, DataDog, etc)
   */
  exportForExternalTools() {
    return {
      format: 'w3c-trace-context',
      data: this.collector.export(),
    };
  }
}

export default {
  TraceContext,
  TracedLog,
  SpanMetrics,
  TraceCollector,
  Fortune5Tracing,
};

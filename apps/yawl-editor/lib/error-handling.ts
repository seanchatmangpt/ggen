/**
 * Comprehensive Error Handling & Recovery
 * Central error management for all 2028+ components
 */

import { z } from 'zod'

/**
 * Error types
 */
export enum ErrorType {
  VALIDATION_ERROR = 'VALIDATION_ERROR',
  NOT_FOUND = 'NOT_FOUND',
  UNAUTHORIZED = 'UNAUTHORIZED',
  FORBIDDEN = 'FORBIDDEN',
  CONFLICT = 'CONFLICT',
  INTERNAL_ERROR = 'INTERNAL_ERROR',
  SERVICE_UNAVAILABLE = 'SERVICE_UNAVAILABLE',
  TIMEOUT = 'TIMEOUT',
  RATE_LIMIT = 'RATE_LIMIT',
  UNKNOWN = 'UNKNOWN',
}

/**
 * Error severity
 */
export enum ErrorSeverity {
  LOW = 'LOW',
  MEDIUM = 'MEDIUM',
  HIGH = 'HIGH',
  CRITICAL = 'CRITICAL',
}

/**
 * Platform Error class
 */
export class PlatformError extends Error {
  type: ErrorType
  severity: ErrorSeverity
  statusCode: number
  details?: any
  timestamp: Date
  id: string

  constructor(
    message: string,
    type: ErrorType = ErrorType.UNKNOWN,
    severity: ErrorSeverity = ErrorSeverity.MEDIUM,
    statusCode: number = 500,
    details?: any
  ) {
    super(message)
    this.type = type
    this.severity = severity
    this.statusCode = statusCode
    this.details = details
    this.timestamp = new Date()
    this.id = `err-${Date.now()}-${Math.random().toString(36).slice(2, 9)}`

    Object.setPrototypeOf(this, PlatformError.prototype)
  }

  toJSON() {
    return {
      id: this.id,
      message: this.message,
      type: this.type,
      severity: this.severity,
      statusCode: this.statusCode,
      details: this.details,
      timestamp: this.timestamp.toISOString(),
    }
  }
}

/**
 * Validation error
 */
export class ValidationError extends PlatformError {
  constructor(message: string, details?: any) {
    super(
      message,
      ErrorType.VALIDATION_ERROR,
      ErrorSeverity.LOW,
      400,
      details
    )
    Object.setPrototypeOf(this, ValidationError.prototype)
  }
}

/**
 * Not found error
 */
export class NotFoundError extends PlatformError {
  constructor(message: string, resource?: string) {
    super(
      message,
      ErrorType.NOT_FOUND,
      ErrorSeverity.LOW,
      404,
      { resource }
    )
    Object.setPrototypeOf(this, NotFoundError.prototype)
  }
}

/**
 * Conflict error
 */
export class ConflictError extends PlatformError {
  constructor(message: string, details?: any) {
    super(
      message,
      ErrorType.CONFLICT,
      ErrorSeverity.MEDIUM,
      409,
      details
    )
    Object.setPrototypeOf(this, ConflictError.prototype)
  }
}

/**
 * Service unavailable error
 */
export class ServiceUnavailableError extends PlatformError {
  constructor(message: string = 'Service temporarily unavailable') {
    super(
      message,
      ErrorType.SERVICE_UNAVAILABLE,
      ErrorSeverity.HIGH,
      503
    )
    Object.setPrototypeOf(this, ServiceUnavailableError.prototype)
  }
}

/**
 * Timeout error
 */
export class TimeoutError extends PlatformError {
  constructor(message: string, duration?: number) {
    super(
      message,
      ErrorType.TIMEOUT,
      ErrorSeverity.MEDIUM,
      504,
      { duration }
    )
    Object.setPrototypeOf(this, TimeoutError.prototype)
  }
}

/**
 * Error handler
 */
export class ErrorHandler {
  private errorLog: PlatformError[] = []
  private maxLogSize: number = 1000
  private errorCallbacks: Map<ErrorType, Function[]> = new Map()

  /**
   * Handle error
   */
  handle(error: any, context?: any): PlatformError {
    let platformError: PlatformError

    if (error instanceof PlatformError) {
      platformError = error
    } else if (error instanceof z.ZodError) {
      platformError = new ValidationError(
        'Validation failed',
        error.errors.map((e) => ({
          path: e.path.join('.'),
          message: e.message,
          code: e.code,
        }))
      )
    } else if (error instanceof TypeError) {
      platformError = new PlatformError(
        error.message,
        ErrorType.INTERNAL_ERROR,
        ErrorSeverity.HIGH
      )
    } else if (error instanceof Error) {
      platformError = new PlatformError(
        error.message,
        ErrorType.UNKNOWN,
        ErrorSeverity.MEDIUM,
        500,
        { stack: error.stack, context }
      )
    } else {
      platformError = new PlatformError(
        String(error),
        ErrorType.UNKNOWN,
        ErrorSeverity.MEDIUM
      )
    }

    this.logError(platformError)
    this.triggerCallbacks(platformError)

    return platformError
  }

  /**
   * Log error
   */
  private logError(error: PlatformError): void {
    this.errorLog.push(error)

    if (this.errorLog.length > this.maxLogSize) {
      this.errorLog.shift()
    }

    console.error(`[${error.id}] ${error.type}: ${error.message}`)
  }

  /**
   * Register error callback
   */
  onError(type: ErrorType, callback: Function): void {
    if (!this.errorCallbacks.has(type)) {
      this.errorCallbacks.set(type, [])
    }
    this.errorCallbacks.get(type)!.push(callback)
  }

  /**
   * Trigger callbacks
   */
  private triggerCallbacks(error: PlatformError): void {
    const callbacks = this.errorCallbacks.get(error.type) || []
    callbacks.forEach((callback) => {
      try {
        callback(error)
      } catch (err) {
        console.error('Error in error callback:', err)
      }
    })
  }

  /**
   * Get error log
   */
  getErrorLog(limit: number = 100): PlatformError[] {
    return this.errorLog.slice(-limit)
  }

  /**
   * Get error statistics
   */
  getStats() {
    const grouped = new Map<ErrorType, number>()

    this.errorLog.forEach((error) => {
      grouped.set(error.type, (grouped.get(error.type) || 0) + 1)
    })

    return {
      totalErrors: this.errorLog.length,
      byType: Object.fromEntries(grouped),
      recentErrors: this.getErrorLog(10).map((e) => ({
        id: e.id,
        type: e.type,
        message: e.message,
        timestamp: e.timestamp,
      })),
    }
  }
}

/**
 * Retry strategy
 */
export class RetryStrategy {
  private maxAttempts: number
  private delayMs: number
  private backoffMultiplier: number

  constructor(
    maxAttempts: number = 3,
    delayMs: number = 1000,
    backoffMultiplier: number = 2
  ) {
    this.maxAttempts = maxAttempts
    this.delayMs = delayMs
    this.backoffMultiplier = backoffMultiplier
  }

  /**
   * Execute with retry
   */
  async execute<T>(
    fn: () => Promise<T>,
    context?: string
  ): Promise<T> {
    let lastError: Error | null = null

    for (let attempt = 1; attempt <= this.maxAttempts; attempt++) {
      try {
        return await fn()
      } catch (error) {
        lastError = error instanceof Error ? error : new Error(String(error))

        if (attempt < this.maxAttempts) {
          const delay = this.delayMs * Math.pow(this.backoffMultiplier, attempt - 1)
          console.log(`⏳ Retry ${attempt}/${this.maxAttempts} after ${delay}ms${context ? ` (${context})` : ''}`)
          await new Promise((resolve) => setTimeout(resolve, delay))
        }
      }
    }

    throw new PlatformError(
      `Failed after ${this.maxAttempts} attempts: ${lastError?.message}`,
      ErrorType.INTERNAL_ERROR,
      ErrorSeverity.HIGH,
      500,
      { context, attempts: this.maxAttempts }
    )
  }

  /**
   * Execute with timeout
   */
  async executeWithTimeout<T>(
    fn: () => Promise<T>,
    timeoutMs: number = 5000
  ): Promise<T> {
    return Promise.race([
      fn(),
      new Promise<T>((_, reject) =>
        setTimeout(
          () => reject(new TimeoutError(`Operation timed out after ${timeoutMs}ms`, timeoutMs)),
          timeoutMs
        )
      ),
    ])
  }
}

/**
 * Global error handler instance
 */
let globalErrorHandler: ErrorHandler | null = null

/**
 * Get error handler
 */
export function getErrorHandler(): ErrorHandler {
  if (!globalErrorHandler) {
    globalErrorHandler = new ErrorHandler()
  }
  return globalErrorHandler
}

/**
 * Validation helpers
 */
export const ValidationSchemas = {
  swarmId: z.string().min(1).max(100),
  agentId: z.string().min(1).max(100),
  taskId: z.string().min(1).max(100),
  priority: z.enum(['low', 'normal', 'high', 'critical']),
  status: z.enum(['idle', 'active', 'processing', 'learning', 'corrupted', 'recovering', 'terminated']),
  taskType: z.string().min(1).max(50),
  messageType: z.enum(['command', 'query', 'response', 'broadcast', 'alert']),
}

/**
 * Safe API call wrapper
 */
export async function safeApiCall<T>(
  fn: () => Promise<T>,
  errorContext?: string
): Promise<{ success: boolean; data?: T; error?: PlatformError }> {
  try {
    const data = await fn()
    return { success: true, data }
  } catch (error) {
    const handler = getErrorHandler()
    const platformError = handler.handle(error, errorContext)
    return { success: false, error: platformError }
  }
}

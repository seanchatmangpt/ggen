/**
 * ggen-paas CLI - Logger Utility
 *
 * Provides structured logging with color support and log aggregation.
 */

export class Logger {
  constructor(options = {}) {
    this.name = options.name || 'CLI';
    this.verbose = options.verbose || false;
    this.quiet = options.quiet || false;
    this.logs = [];
    this.startTime = Date.now();
  }

  /**
   * Log info message
   */
  info(message) {
    const timestamp = this._getTimestamp();
    const logEntry = `[${timestamp}] [${this.name}] ℹ ${message}`;
    this.logs.push({ level: 'info', message, timestamp });

    if (!this.quiet) {
      console.log(logEntry);
    }
  }

  /**
   * Log success message
   */
  success(message) {
    const timestamp = this._getTimestamp();
    const logEntry = `[${timestamp}] [${this.name}] ✓ ${message}`;
    this.logs.push({ level: 'success', message, timestamp });

    if (!this.quiet) {
      console.log(logEntry);
    }
  }

  /**
   * Log warning message
   */
  warn(message) {
    const timestamp = this._getTimestamp();
    const logEntry = `[${timestamp}] [${this.name}] ⚠ ${message}`;
    this.logs.push({ level: 'warn', message, timestamp });

    if (!this.quiet) {
      console.warn(logEntry);
    }
  }

  /**
   * Log error message
   */
  error(message) {
    const timestamp = this._getTimestamp();
    const logEntry = `[${timestamp}] [${this.name}] ✗ ${message}`;
    this.logs.push({ level: 'error', message, timestamp });

    if (!this.quiet) {
      console.error(logEntry);
    }
  }

  /**
   * Log debug message (only if verbose)
   */
  debug(message) {
    if (!this.verbose) {
      return;
    }

    const timestamp = this._getTimestamp();
    const logEntry = `[${timestamp}] [${this.name}] ⚙ ${message}`;
    this.logs.push({ level: 'debug', message, timestamp });

    if (!this.quiet) {
      console.log(logEntry);
    }
  }

  /**
   * Get all logs as array
   */
  getLogs() {
    return this.logs;
  }

  /**
   * Get logs summary
   */
  getSummary() {
    const duration = Date.now() - this.startTime;
    const counts = {
      info: this.logs.filter(l => l.level === 'info').length,
      success: this.logs.filter(l => l.level === 'success').length,
      warn: this.logs.filter(l => l.level === 'warn').length,
      error: this.logs.filter(l => l.level === 'error').length,
      debug: this.logs.filter(l => l.level === 'debug').length,
    };

    return {
      name: this.name,
      duration,
      totalLogs: this.logs.length,
      counts,
    };
  }

  /**
   * Get current timestamp
   * @private
   */
  _getTimestamp() {
    return new Date().toISOString();
  }
}

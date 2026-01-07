/**
 * ANDON SIGNALS - TOYOTA PRODUCTION SYSTEM QUALITY GATES
 * ============================================================================
 * Stop-the-line alert system for instant quality feedback
 * Integrated into ggen five-stage pipeline and Fortune 5 benchmarks
 *
 * Inspired by Toyota's Andon Cord: Pull to stop production when quality fails
 * ============================================================================
 */

import { EventEmitter } from 'events';

export class AndonSignal extends EventEmitter {
  constructor() {
    super();
    this.signals = {
      GREEN: { level: 0, name: 'GREEN', meaning: 'All systems normal, proceed' },
      YELLOW: { level: 1, name: 'YELLOW', meaning: 'Warning condition detected, investigate' },
      RED: { level: 2, name: 'RED', meaning: 'Critical failure, STOP execution' },
    };

    this.currentSignal = this.signals.GREEN;
    this.violations = [];
    this.lastChangeTime = Date.now();
  }

  /**
   * Record a violation and escalate signal if needed
   */
  recordViolation(type, severity, message, metadata = {}) {
    const violation = {
      type,
      severity, // 'info', 'warning', 'error', 'critical'
      message,
      metadata,
      timestamp: Date.now(),
    };

    this.violations.push(violation);

    // Escalate signal based on severity
    if (severity === 'critical') {
      this.setSignal(this.signals.RED);
    } else if (severity === 'error' && this.currentSignal.level < 1) {
      this.setSignal(this.signals.YELLOW);
    }

    this.emit('violation', violation);
  }

  /**
   * Set current signal (only escalate, never downgrade automatically)
   */
  setSignal(signal) {
    if (signal.level > this.currentSignal.level) {
      const oldSignal = this.currentSignal;
      this.currentSignal = signal;
      this.lastChangeTime = Date.now();

      this.emit('signal-change', {
        from: oldSignal,
        to: signal,
        timestamp: Date.now(),
      });

      if (signal.level === 2) {
        // RED signal: Stop execution immediately
        this.emit('stop-the-line', {
          reason: 'RED signal triggered',
          violations: this.violations,
        });
      }
    }
  }

  /**
   * Manual reset to GREEN (requires explicit action)
   */
  reset() {
    this.currentSignal = this.signals.GREEN;
    this.violations = [];
    this.emit('reset');
  }

  /**
   * Get current status
   */
  getStatus() {
    return {
      signal: this.currentSignal,
      violationCount: this.violations.length,
      uptime: Date.now() - this.lastChangeTime,
      violations: this.violations,
      shouldContinue: this.currentSignal.level < 2, // Only RED stops
    };
  }

  /**
   * Report status to console with colors
   */
  printStatus() {
    const status = this.getStatus();
    const signalEmoji = {
      GREEN: '🟢',
      YELLOW: '🟡',
      RED: '🔴',
    };

    console.log(`\n${signalEmoji[status.signal.name]} ANDON: ${status.signal.name}`);
    console.log(`   Violations: ${status.violationCount}`);
    console.log(`   Status: ${status.signal.meaning}`);

    if (status.violations.length > 0) {
      console.log('\n   Recent violations:');
      status.violations.slice(-5).forEach(v => {
        console.log(`   • [${v.severity.toUpperCase()}] ${v.message}`);
      });
    }
  }
}

/**
 * FORTUNE 5 QUALITY GATES - Specific to benchmarks
 */
export class Fortune5AndonSignals {
  constructor() {
    this.andon = new AndonSignal();
    this.checks = {
      cli_startup: { threshold: 100, unit: 'ms', violated: false },
      template_rendering: { threshold: 1000, unit: 'ms', violated: false },
      rdf_query: { threshold: 100, unit: 'ms', violated: false },
      memory_peak: { threshold: 500, unit: 'MB', violated: false },
      memory_leak: { threshold: 5, unit: 'MB/snapshot', violated: false },
      concurrency_scaling_2core: { threshold: 1.9, unit: 'ratio', violated: false },
    };
  }

  /**
   * Check CLI startup timing
   */
  checkCliStartup(duration) {
    const threshold = this.checks.cli_startup.threshold;
    if (duration > threshold) {
      this.andon.recordViolation(
        'CLI_STARTUP',
        duration > threshold * 1.2 ? 'critical' : 'error',
        `CLI startup took ${duration}ms, threshold is ${threshold}ms`,
        { actual: duration, threshold }
      );
      this.checks.cli_startup.violated = true;
      return false;
    }
    return true;
  }

  /**
   * Check template rendering performance
   */
  checkTemplateRendering(duration) {
    const threshold = this.checks.template_rendering.threshold;
    if (duration > threshold * 1.5) {
      this.andon.recordViolation(
        'TEMPLATE_RENDERING',
        'critical',
        `Template rendering took ${duration}ms, critical threshold is ${threshold * 1.5}ms`,
        { actual: duration, threshold }
      );
      this.checks.template_rendering.violated = true;
      return false;
    } else if (duration > threshold) {
      this.andon.recordViolation(
        'TEMPLATE_RENDERING',
        'error',
        `Template rendering took ${duration}ms, target is ${threshold}ms`,
        { actual: duration, threshold }
      );
      return false;
    }
    return true;
  }

  /**
   * Check RDF query latency
   */
  checkRdfQuery(p90Duration) {
    const threshold = this.checks.rdf_query.threshold;
    if (p90Duration > threshold) {
      this.andon.recordViolation(
        'RDF_QUERY',
        'error',
        `RDF query p90 is ${p90Duration}ms, SLA is ${threshold}ms`,
        { actual: p90Duration, threshold }
      );
      this.checks.rdf_query.violated = true;
      return false;
    }
    return true;
  }

  /**
   * Check memory usage
   */
  checkMemoryUsage(peakMB) {
    const threshold = this.checks.memory_peak.threshold;
    if (peakMB > threshold) {
      const severity = peakMB > threshold * 1.1 ? 'critical' : 'error';
      this.andon.recordViolation(
        'MEMORY_PEAK',
        severity,
        `Memory peak is ${peakMB}MB, limit is ${threshold}MB`,
        { actual: peakMB, threshold }
      );
      this.checks.memory_peak.violated = true;
      return false;
    }
    return true;
  }

  /**
   * Check for memory leaks
   */
  checkMemoryLeak(growthPerSnapshot) {
    const threshold = this.checks.memory_leak.threshold;
    if (growthPerSnapshot > threshold) {
      this.andon.recordViolation(
        'MEMORY_LEAK',
        'critical',
        `Memory growth is ${growthPerSnapshot}MB/snapshot, limit is ${threshold}MB/snapshot`,
        { actual: growthPerSnapshot, threshold }
      );
      this.checks.memory_leak.violated = true;
      return false;
    }
    return true;
  }

  /**
   * Check concurrency scaling efficiency
   */
  checkConcurrencyScaling(cores, scalingRatio) {
    const requiredRatio = cores === 2 ? 1.9 : cores === 4 ? 3.8 : cores === 8 ? 7.5 : cores - 0.5;
    if (scalingRatio < requiredRatio) {
      this.andon.recordViolation(
        'CONCURRENCY_SCALING',
        'error',
        `${cores}-core scaling is ${scalingRatio.toFixed(2)}x, target is ${requiredRatio}x`,
        { actual: scalingRatio, threshold: requiredRatio, cores }
      );
      this.checks[`concurrency_scaling_${cores}core`].violated = true;
      return false;
    }
    return true;
  }

  /**
   * Get overall signal status
   */
  getSignal() {
    return this.andon.currentSignal;
  }

  /**
   * Print comprehensive report
   */
  printReport() {
    console.log('\n' + '='.repeat(80));
    console.log('ANDON SIGNAL REPORT - FORTUNE 5 QUALITY GATES');
    console.log('='.repeat(80));

    const status = this.andon.getStatus();

    // Signal status
    const signalEmoji = { GREEN: '🟢', YELLOW: '🟡', RED: '🔴' };
    console.log(`\n${signalEmoji[status.signal.name]} Current Signal: ${status.signal.name}`);
    console.log(`   ${status.signal.meaning}`);

    // Check results
    console.log('\nQuality Gate Status:');
    for (const [check, result] of Object.entries(this.checks)) {
      const emoji = result.violated ? '❌' : '✓';
      console.log(`   ${emoji} ${check}: ${result.threshold} ${result.unit}`);
    }

    // Violations
    if (status.violations.length > 0) {
      console.log(`\nViolations (${status.violations.length}):`);
      status.violations.forEach(v => {
        const emoji = v.severity === 'critical' ? '🔴' : v.severity === 'error' ? '🟡' : 'ℹ️';
        console.log(`   ${emoji} [${v.severity.toUpperCase()}] ${v.message}`);
      });
    } else {
      console.log('\n✓ All quality gates passing');
    }

    console.log('\n' + '='.repeat(80));
    if (status.signal.level === 2) {
      console.log('🛑 STOP THE LINE - Critical issues detected');
    } else if (status.signal.level === 1) {
      console.log('⚠️  WARNING - Investigate before proceeding');
    } else {
      console.log('✓ PROCEED - All systems nominal');
    }
    console.log('='.repeat(80) + '\n');
  }
}

export default AndonSignal;

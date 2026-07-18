/**
 * GOLDEN SIGNALS - OBSERVABILITY FRAMEWORK
 * ============================================================================
 * SRE book identifies 4 key metrics that matter for production systems:
 * 1. Latency: Request latency (how long does it take?)
 * 2. Traffic: Request rate (how much load?)
 * 3. Errors: Error rate (how many fail?)
 * 4. Saturation: Resource utilization (how full is the system?)
 *
 * Applied to Fortune 5: Measure these 4 signals for each pattern
 * ============================================================================
 */

/**
 * Golden Signal tracker
 */
export class GoldenSignal {
  constructor(name, unit, target) {
    this.name = name;
    this.unit = unit;
    this.target = target; // SLA target
    this.measurements = [];
  }

  /**
   * Record measurement
   */
  record(value, timestamp = Date.now()) {
    this.measurements.push({ value, timestamp });
  }

  /**
   * Calculate statistics
   */
  getStats() {
    if (this.measurements.length === 0) {
      return null;
    }

    const values = this.measurements.map(m => m.value);
    values.sort((a, b) => a - b);

    const sum = values.reduce((a, b) => a + b, 0);
    const mean = sum / values.length;

    const p50 = values[Math.floor(values.length * 0.5)];
    const p95 = values[Math.floor(values.length * 0.95)];
    const p99 = values[Math.floor(values.length * 0.99)];

    return {
      count: values.length,
      min: values[0],
      max: values[values.length - 1],
      mean,
      p50,
      p95,
      p99,
      target: this.target,
      targetMet: p95 <= this.target,
    };
  }

  /**
   * Get trend (improving or degrading?)
   */
  getTrend(windowSize = 100) {
    if (this.measurements.length < windowSize * 2) {
      return 'insufficient_data';
    }

    const recent = this.measurements.slice(-windowSize);
    const previous = this.measurements.slice(-windowSize * 2, -windowSize);

    const recentAvg = recent.reduce((sum, m) => sum + m.value, 0) / recent.length;
    const previousAvg = previous.reduce((sum, m) => sum + m.value, 0) / previous.length;

    if (recentAvg < previousAvg * 0.95) {
      return 'improving';
    } else if (recentAvg > previousAvg * 1.05) {
      return 'degrading';
    } else {
      return 'stable';
    }
  }
}

/**
 * The 4 Golden Signals
 */
export class FourGoldenSignals {
  constructor() {
    this.latency = new GoldenSignal('Latency', 'ms', 100);
    this.traffic = new GoldenSignal('Traffic', 'req/s', 1000);
    this.errors = new GoldenSignal('Error Rate', '%', 0.1); // 0.1% error SLA
    this.saturation = new GoldenSignal('Saturation', '%', 80); // 80% utilization target
  }

  /**
   * Record observations
   */
  recordLatency(ms) {
    this.latency.record(ms);
  }

  recordTraffic(requestsPerSecond) {
    this.traffic.record(requestsPerSecond);
  }

  recordErrors(errorPercentage) {
    this.errors.record(errorPercentage);
  }

  recordSaturation(percentUtilized) {
    this.saturation.record(percentUtilized);
  }

  /**
   * Health check based on golden signals
   */
  getHealth() {
    const latencyStats = this.latency.getStats();
    const errorStats = this.errors.getStats();
    const saturationStats = this.saturation.getStats();

    const issues = [];

    if (latencyStats && !latencyStats.targetMet) {
      issues.push(`Latency p95=${latencyStats.p95}ms exceeds ${latencyStats.target}ms`);
    }

    if (errorStats && !errorStats.targetMet) {
      issues.push(`Error rate ${errorStats.mean}% exceeds ${errorStats.target}%`);
    }

    if (saturationStats && saturationStats.mean > 90) {
      issues.push(`Saturation ${saturationStats.mean}% is critical (>90%)`);
    }

    return {
      healthy: issues.length === 0,
      issues,
      timestamp: Date.now(),
    };
  }

  /**
   * Get comprehensive signal report
   */
  getReport() {
    return {
      latency: {
        name: 'Latency',
        stats: this.latency.getStats(),
        trend: this.latency.getTrend(),
      },
      traffic: {
        name: 'Traffic',
        stats: this.traffic.getStats(),
        trend: this.traffic.getTrend(),
      },
      errors: {
        name: 'Errors',
        stats: this.errors.getStats(),
        trend: this.errors.getTrend(),
      },
      saturation: {
        name: 'Saturation',
        stats: this.saturation.getStats(),
        trend: this.saturation.getTrend(),
      },
      health: this.getHealth(),
    };
  }

  /**
   * Print human-readable report
   */
  printReport() {
    const report = this.getReport();

    console.log('\n' + '='.repeat(80));
    console.log('GOLDEN SIGNALS REPORT');
    console.log('='.repeat(80));

    Object.values(report).forEach(signal => {
      if (signal.stats) {
        const stats = signal.stats;
        const trendEmoji = {
          improving: '📈',
          degrading: '📉',
          stable: '→',
          insufficient_data: '?',
        };

        console.log(
          `\n${signal.name}: ${stats.mean.toFixed(2)} ${stats.stats.unit} ` +
          `${trendEmoji[signal.trend]}`
        );
        console.log(
          `  p50: ${stats.p50.toFixed(2)}, p95: ${stats.p95.toFixed(2)}, p99: ${stats.p99.toFixed(2)}`
        );
        console.log(`  Target: ${stats.target} (${stats.targetMet ? '✓' : '✗'})`);
      }
    });

    console.log('\nHealth: ' + (report.health.healthy ? '✓ HEALTHY' : '✗ ISSUES'));
    report.health.issues.forEach(issue => {
      console.log(`  - ${issue}`);
    });

    console.log('='.repeat(80) + '\n');
  }
}

/**
 * ERROR BUDGET - Quantified reliability targets
 * ============================================================================
 * How much failure are we allowed before we breach SLO?
 *
 * Example:
 * - SLO: 99.9% availability
 * - Period: 30 days
 * - Error budget: 43 minutes of downtime allowed
 * ============================================================================
 */

export class ErrorBudget {
  constructor(sloPercentage, periodDays) {
    this.sloPercentage = sloPercentage; // e.g., 99.9
    this.periodDays = periodDays;

    // Calculate budget in minutes
    const uptime = sloPercentage / 100;
    const downtime = 1 - uptime;
    const minutesInPeriod = periodDays * 24 * 60;
    this.budgetMinutes = minutesInPeriod * downtime;

    this.spent = 0; // Minutes of failures
    this.startTime = Date.now();
  }

  /**
   * Record a failure
   */
  recordFailure(durationMs) {
    this.spent += durationMs / 1000 / 60; // Convert to minutes
  }

  /**
   * Record multiple failures
   */
  recordFailures(failures) {
    failures.forEach(duration => this.recordFailure(duration));
  }

  /**
   * Get budget status
   */
  getStatus() {
    const remaining = this.budgetMinutes - this.spent;
    const percentRemaining = (remaining / this.budgetMinutes) * 100;
    const hoursElapsed = (Date.now() - this.startTime) / 1000 / 60 / 60;

    return {
      slo: `${this.sloPercentage}%`,
      period: `${this.periodDays} days`,
      totalBudget: this.budgetMinutes.toFixed(1),
      spent: this.spent.toFixed(1),
      remaining: remaining.toFixed(1),
      percentRemaining: percentRemaining.toFixed(1),
      breached: remaining <= 0,
      burnRate: this.spent / hoursElapsed,
    };
  }

  /**
   * Should we deploy risky changes?
   * Only if we have budget cushion (>50% remaining)
   */
  canDeployRiskyChanges() {
    const status = this.getStatus();
    return parseFloat(status.percentRemaining) > 50;
  }

  /**
   * Burndown projection
   */
  projectBudgetDepletion() {
    const status = this.getStatus();
    const remaining = parseFloat(status.remaining);
    const burnRate = status.burnRate;

    if (burnRate <= 0) return null;

    const hoursUntilDepleted = remaining / 60 / burnRate;
    const daysUntilDepleted = hoursUntilDepleted / 24;

    return {
      hoursUntilDepleted: hoursUntilDepleted.toFixed(1),
      daysUntilDepleted: daysUntilDepleted.toFixed(1),
      projectedDepletion: new Date(
        Date.now() + hoursUntilDepleted * 60 * 60 * 1000
      ).toISOString(),
    };
  }

  /**
   * Print budget status
   */
  printStatus() {
    const status = this.getStatus();
    const projection = this.projectBudgetDepletion();

    console.log('\n' + '='.repeat(60));
    console.log('ERROR BUDGET STATUS');
    console.log('='.repeat(60));
    console.log(`SLO: ${status.slo} over ${status.period}`);
    console.log(`Total Budget: ${status.totalBudget} minutes`);
    console.log(`Spent: ${status.spent} minutes`);
    console.log(`Remaining: ${status.remaining} minutes (${status.percentRemaining}%)`);
    console.log(`Burn Rate: ${status.burnRate.toFixed(3)} min/hour`);

    if (status.breached) {
      console.log('\n🔴 ERROR BUDGET BREACHED - SLO is violated');
    } else if (parseFloat(status.percentRemaining) < 10) {
      console.log('\n⚠️  WARNING - Error budget critically low (<10%)');
    } else if (parseFloat(status.percentRemaining) < 50) {
      console.log('\n🟡 CAUTION - Error budget usage increasing');
    } else {
      console.log('\n✓ Error budget healthy (>50% remaining)');
    }

    if (projection) {
      console.log(`\nProjected Depletion: ${projection.daysUntilDepleted} days`);
    }

    console.log('='.repeat(60) + '\n');
  }
}

/**
 * Fortune 5 SLO Tracker
 */
export class Fortune5SloTracker {
  constructor() {
    this.patterns = {
      cli_startup: new ErrorBudget(99.9, 30), // 99.9% for 30 days
      template_rendering: new ErrorBudget(99.5, 30),
      rdf_query: new ErrorBudget(99.9, 30),
      memory_usage: new ErrorBudget(99.99, 30),
      concurrent_ops: new ErrorBudget(99.9, 30),
    };

    this.signals = new FourGoldenSignals();
  }

  /**
   * Record a pattern failure
   */
  recordPatternFailure(pattern, durationMs) {
    if (this.patterns[pattern]) {
      this.patterns[pattern].recordFailure(durationMs);
    }
  }

  /**
   * Get overall SLO status
   */
  getOverallStatus() {
    const statuses = {};
    for (const [pattern, budget] of Object.entries(this.patterns)) {
      statuses[pattern] = budget.getStatus();
    }

    const breached = Object.values(statuses).some(s => s.breached);

    return {
      patterns: statuses,
      overallBreached: breached,
      timestamp: new Date().toISOString(),
    };
  }

  /**
   * Print full report
   */
  printFullReport() {
    console.log('\n' + '#'.repeat(80));
    console.log('# FORTUNE 5 SLO TRACKER - COMPLETE REPORT');
    console.log('#'.repeat(80));

    this.signals.printReport();

    const status = this.getOverallStatus();
    console.log('\nPER-PATTERN ERROR BUDGETS:');
    Object.entries(status.patterns).forEach(([pattern, budget]) => {
      const indicator = budget.breached ? '❌' : '✓';
      console.log(`${indicator} ${pattern}: ${budget.remaining} min remaining`);
    });

    if (status.overallBreached) {
      console.log('\n🔴 SYSTEM SLO BREACHED');
    } else {
      console.log('\n✓ All SLOs within budget');
    }

    console.log('#'.repeat(80) + '\n');
  }
}

export default {
  GoldenSignal,
  FourGoldenSignals,
  ErrorBudget,
  Fortune5SloTracker,
};

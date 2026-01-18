/**
 * FORTUNE 5 PERFORMANCE METRICS
 * ============================================================================
 * Real-time performance metrics collector for Bree scheduler
 * Measures all 5 Fortune 500 enterprise patterns:
 * 1. CLI Startup (<100ms)
 * 2. Template Rendering (<1s typical)
 * 3. RDF Query Performance (<100ms p90)
 * 4. Memory Usage (<500MB peak)
 * 5. Concurrent Operations (linear scaling)
 * ============================================================================
 */

import { performance } from 'perf_hooks';
import os from 'os';

/**
 * Metrics collector for performance tracking
 */
export class Fortune5Metrics {
  constructor() {
    // Pattern 1: CLI Startup
    this.cliStartupTimes = [];

    // Pattern 2: Template Rendering
    this.templateRenderTimes = [];

    // Pattern 3: RDF Query Performance
    this.rdfQueryTimes = [];

    // Pattern 4: Memory Usage
    this.memorySnapshots = [];

    // Pattern 5: Concurrent Operations
    this.concurrencyThroughput = [];

    this.startTime = Date.now();
  }

  // =========================================================================
  // PATTERN 1: CLI STARTUP PERFORMANCE
  // =========================================================================

  recordCliStartup(command, duration) {
    this.cliStartupTimes.push({
      command,
      duration,
      timestamp: Date.now(),
    });
  }

  getCliStartupStats() {
    if (this.cliStartupTimes.length === 0) return null;

    const durations = this.cliStartupTimes.map(t => t.duration);
    return {
      count: durations.length,
      mean: durations.reduce((a, b) => a + b) / durations.length,
      median: this.percentile(durations, 50),
      p90: this.percentile(durations, 90),
      p99: this.percentile(durations, 99),
      min: Math.min(...durations),
      max: Math.max(...durations),
      targetSLA: 100, // ms
      slaPass: this.percentile(durations, 90) <= 100,
    };
  }

  // =========================================================================
  // PATTERN 2: TEMPLATE RENDERING AT SCALE
  // =========================================================================

  recordTemplateRender(templateName, jobCount, duration) {
    this.templateRenderTimes.push({
      templateName,
      jobCount,
      duration,
      timestamp: Date.now(),
    });
  }

  getTemplateRenderingStats() {
    if (this.templateRenderTimes.length === 0) return null;

    const durations = this.templateRenderTimes.map(t => t.duration);
    const throughput = this.templateRenderTimes.map(t => t.jobCount);

    return {
      count: durations.length,
      totalFiles: throughput.reduce((a, b) => a + b, 0),
      filesPerSecond: (throughput.reduce((a, b) => a + b, 0) * 1000) /
                     durations.reduce((a, b) => a + b, 1),
      meanTime: durations.reduce((a, b) => a + b) / durations.length,
      p90: this.percentile(durations, 90),
      targetSLA: {
        typical: 1000,    // ms for typical
        large: 5000,      // ms for large
      },
      slaPass: {
        typical: this.percentile(durations, 90) <= 1000,
        large: this.percentile(durations, 90) <= 5000,
      },
    };
  }

  // =========================================================================
  // PATTERN 3: RDF QUERY PERFORMANCE
  // =========================================================================

  recordRdfQuery(queryName, complexity, duration) {
    this.rdfQueryTimes.push({
      queryName,
      complexity,
      duration,
      timestamp: Date.now(),
    });
  }

  getRdfQueryStats() {
    if (this.rdfQueryTimes.length === 0) return null;

    const durations = this.rdfQueryTimes.map(t => t.duration);

    return {
      count: durations.length,
      mean: durations.reduce((a, b) => a + b) / durations.length,
      median: this.percentile(durations, 50),
      p90: this.percentile(durations, 90),
      p99: this.percentile(durations, 99),
      targetSLA: 100, // ms at p90
      slaPass: this.percentile(durations, 90) <= 100,
      complexityBreakdown: {
        simple: this.rdfQueryTimes
          .filter(q => q.complexity === 'simple')
          .map(q => q.duration),
        moderate: this.rdfQueryTimes
          .filter(q => q.complexity === 'moderate')
          .map(q => q.duration),
        complex: this.rdfQueryTimes
          .filter(q => q.complexity === 'complex')
          .map(q => q.duration),
      },
    };
  }

  // =========================================================================
  // PATTERN 4: MEMORY USAGE MANAGEMENT
  // =========================================================================

  recordMemorySnapshot(label) {
    const usage = process.memoryUsage();
    this.memorySnapshots.push({
      label,
      heapUsed: usage.heapUsed / 1024 / 1024,  // MB
      heapTotal: usage.heapTotal / 1024 / 1024,
      rss: usage.rss / 1024 / 1024,             // Resident Set Size
      external: usage.external / 1024 / 1024,
      timestamp: Date.now(),
    });
  }

  getMemoryStats() {
    if (this.memorySnapshots.length === 0) return null;

    const heapUsed = this.memorySnapshots.map(s => s.heapUsed);
    const rss = this.memorySnapshots.map(s => s.rss);

    return {
      snapshots: this.memorySnapshots.length,
      baseline: this.memorySnapshots[0] || null,
      current: this.memorySnapshots[this.memorySnapshots.length - 1] || null,
      heapUsed: {
        mean: heapUsed.reduce((a, b) => a + b) / heapUsed.length,
        peak: Math.max(...heapUsed),
        min: Math.min(...heapUsed),
      },
      rss: {
        mean: rss.reduce((a, b) => a + b) / rss.length,
        peak: Math.max(...rss),
        min: Math.min(...rss),
      },
      targetSLA: {
        baseline: 50,   // MB
        peak: 500,      // MB
      },
      slaPass: {
        baseline: heapUsed[0] <= 50,
        peak: Math.max(...heapUsed) <= 500,
      },
      leakDetection: this.detectMemoryLeak(),
    };
  }

  detectMemoryLeak() {
    if (this.memorySnapshots.length < 3) return 'insufficient_data';

    // Simple leak detection: check if memory is consistently growing
    const recent = this.memorySnapshots.slice(-10);
    const growth = recent.reduce((prev, current, i) => {
      if (i === 0) return 0;
      return prev + (current.heapUsed - recent[i - 1].heapUsed);
    }, 0);

    const avgGrowthPerSnapshot = growth / (recent.length - 1);

    if (avgGrowthPerSnapshot > 5) {
      return 'likely_leak';
    } else if (avgGrowthPerSnapshot > 1) {
      return 'possible_leak';
    } else {
      return 'no_leak_detected';
    }
  }

  // =========================================================================
  // PATTERN 5: CONCURRENT OPERATIONS
  // =========================================================================

  recordConcurrencyThroughput(parallelLevel, jobsCompleted, duration) {
    this.concurrencyThroughput.push({
      parallelLevel,
      jobsCompleted,
      duration,
      jobsPerSecond: (jobsCompleted / duration) * 1000,
      timestamp: Date.now(),
    });
  }

  getConcurrencyStats() {
    if (this.concurrencyThroughput.length === 0) return null;

    // Group by parallel level
    const byLevel = {};
    this.concurrencyThroughput.forEach(t => {
      if (!byLevel[t.parallelLevel]) {
        byLevel[t.parallelLevel] = [];
      }
      byLevel[t.parallelLevel].push(t.jobsPerSecond);
    });

    // Calculate baseline (1-core)
    const baseline = byLevel[1] || [];
    const baselineAvg = baseline.length > 0
      ? baseline.reduce((a, b) => a + b) / baseline.length
      : 0;

    // Calculate scaling for each level
    const scaling = {};
    for (const level of [2, 4, 8]) {
      if (byLevel[level]) {
        const levelAvg = byLevel[level].reduce((a, b) => a + b) / byLevel[level].length;
        scaling[level] = baselineAvg > 0 ? levelAvg / baselineAvg : 0;
      }
    }

    return {
      baseline: {
        throughput: baselineAvg,
        count: baseline.length,
      },
      scaling,
      targetSLA: {
        '2-core': 1.9,  // Should be ≥ 1.9x
        '4-core': 3.8,  // Should be ≥ 3.8x
        '8-core': 7.5,  // Should be ≥ 7.5x
      },
      slaPass: {
        '2-core': (scaling[2] || 0) >= 1.9,
        '4-core': (scaling[4] || 0) >= 3.8,
        '8-core': (scaling[8] || 0) >= 7.5,
      },
      efficiency: {
        '2-core': (scaling[2] || 0) / 2,
        '4-core': (scaling[4] || 0) / 4,
        '8-core': (scaling[8] || 0) / 8,
      },
    };
  }

  // =========================================================================
  // UTILITY METHODS
  // =========================================================================

  percentile(arr, p) {
    if (arr.length === 0) return 0;
    const sorted = [...arr].sort((a, b) => a - b);
    const index = Math.ceil((p / 100) * sorted.length) - 1;
    return sorted[Math.max(0, index)];
  }

  /**
   * Generate comprehensive Fortune 5 report
   */
  generateReport() {
    return {
      timestamp: new Date().toISOString(),
      duration: Date.now() - this.startTime,
      patterns: {
        pattern1_cli_startup: this.getCliStartupStats(),
        pattern2_template_rendering: this.getTemplateRenderingStats(),
        pattern3_rdf_query: this.getRdfQueryStats(),
        pattern4_memory_usage: this.getMemoryStats(),
        pattern5_concurrent_ops: this.getConcurrencyStats(),
      },
    };
  }

  /**
   * Check overall Fortune 500 readiness
   */
  checkFortune500Ready() {
    const stats = this.generateReport();
    const issues = [];

    // Check each pattern
    if (stats.patterns.pattern1_cli_startup) {
      if (!stats.patterns.pattern1_cli_startup.slaPass) {
        issues.push('❌ Pattern 1 (CLI Startup) FAILED - exceeds 100ms SLA');
      } else {
        issues.push('✓ Pattern 1 (CLI Startup) PASS');
      }
    }

    if (stats.patterns.pattern2_template_rendering) {
      const tr = stats.patterns.pattern2_template_rendering;
      if (!tr.slaPass.typical || !tr.slaPass.large) {
        issues.push('❌ Pattern 2 (Template Rendering) FAILED');
      } else {
        issues.push('✓ Pattern 2 (Template Rendering) PASS');
      }
    }

    if (stats.patterns.pattern3_rdf_query) {
      if (!stats.patterns.pattern3_rdf_query.slaPass) {
        issues.push('❌ Pattern 3 (RDF Query) FAILED - exceeds 100ms p90');
      } else {
        issues.push('✓ Pattern 3 (RDF Query) PASS');
      }
    }

    if (stats.patterns.pattern4_memory_usage) {
      const mu = stats.patterns.pattern4_memory_usage;
      if (!mu.slaPass.baseline || !mu.slaPass.peak) {
        issues.push('❌ Pattern 4 (Memory Usage) FAILED');
      } else {
        issues.push('✓ Pattern 4 (Memory Usage) PASS');
      }
    }

    if (stats.patterns.pattern5_concurrent_ops) {
      const co = stats.patterns.pattern5_concurrent_ops;
      if (!co.slaPass['2-core'] || !co.slaPass['4-core'] || !co.slaPass['8-core']) {
        issues.push('❌ Pattern 5 (Concurrent Ops) FAILED - scaling issue');
      } else {
        issues.push('✓ Pattern 5 (Concurrent Ops) PASS');
      }
    }

    const allPass = issues.every(i => i.startsWith('✓'));

    return {
      fortune500Ready: allPass,
      issues,
      timestamp: new Date().toISOString(),
    };
  }

  /**
   * Pretty-print report to console
   */
  printReport() {
    const ready = this.checkFortune500Ready();

    console.log('\n' + '='.repeat(80));
    console.log('FORTUNE 5 PERFORMANCE REPORT');
    console.log('='.repeat(80));

    ready.issues.forEach(issue => {
      console.log(issue);
    });

    console.log('='.repeat(80));
    if (ready.fortune500Ready) {
      console.log('✓ APPLICATION IS FORTUNE 500 READY');
    } else {
      console.log('❌ APPLICATION DOES NOT MEET FORTUNE 500 REQUIREMENTS');
    }
    console.log('='.repeat(80) + '\n');

    const stats = this.generateReport();
    console.log(JSON.stringify(stats, null, 2));
  }
}

export default Fortune5Metrics;

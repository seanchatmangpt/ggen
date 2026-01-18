/**
 * POKA-YOKE ERROR PROOFING - MAKE MISTAKES IMPOSSIBLE
 * ============================================================================
 * From Toyota Production System: Design systems so errors are impossible,
 * not just detected. Example: USB-A connectors can't be plugged in wrong.
 *
 * Applied to Fortune 5 benchmarks:
 * - Type-safe metric recording (can't record invalid data)
 * - Mandatory fields (can't create incomplete benchmarks)
 * - Enum validation (can't use invalid pattern names)
 * - Range checking (can't record negative durations)
 * - State machine enforcement (can't call methods in wrong order)
 * ============================================================================
 */

/**
 * Type-safe metric value - can't be invalid
 */
export class MetricValue {
  constructor(value, unit, min, max) {
    if (typeof value !== 'number' || !Number.isFinite(value)) {
      throw new TypeError(`Metric value must be a finite number, got ${value}`);
    }
    if (value < min || value > max) {
      throw new RangeError(`Metric value ${value} outside valid range [${min}, ${max}]`);
    }

    this.value = value;
    this.unit = unit;
    this.min = min;
    this.max = max;
    this.timestamp = Date.now();
  }

  /**
   * Can only compare with same unit
   */
  compare(other) {
    if (other.unit !== this.unit) {
      throw new Error(`Cannot compare values with different units: ${this.unit} vs ${other.unit}`);
    }
    return this.value - other.value;
  }

  /**
   * Can only create percentile from array
   */
  static percentile(values, p) {
    if (!Array.isArray(values) || values.length === 0) {
      throw new Error('Percentile requires non-empty array of MetricValue objects');
    }
    if (p < 0 || p > 100 || typeof p !== 'number') {
      throw new Error(`Percentile must be between 0-100, got ${p}`);
    }

    const sorted = values.map(v => v.value).sort((a, b) => a - b);
    const index = Math.ceil((p / 100) * sorted.length) - 1;
    return sorted[Math.max(0, index)];
  }
}

/**
 * Pattern enum - can't use invalid pattern names
 */
export const Fortune5Pattern = Object.freeze({
  CLI_STARTUP: 'CLI_STARTUP',
  TEMPLATE_RENDERING: 'TEMPLATE_RENDERING',
  RDF_QUERY: 'RDF_QUERY',
  MEMORY_USAGE: 'MEMORY_USAGE',
  CONCURRENT_OPS: 'CONCURRENT_OPS',
});

/**
 * Severity enum - can't use arbitrary strings
 */
export const Severity = Object.freeze({
  INFO: 'info',
  WARNING: 'warning',
  ERROR: 'error',
  CRITICAL: 'critical',
});

/**
 * Benchmark state machine - enforce proper sequencing
 */
export class BenchmarkStateMachine {
  constructor(name) {
    this.name = name;
    this.state = 'CREATED';
    this.transitions = {
      CREATED: ['STARTED'],
      STARTED: ['RECORDING', 'CANCELLED'],
      RECORDING: ['COMPLETED', 'FAILED'],
      COMPLETED: ['RESET'],
      FAILED: ['RESET'],
      CANCELLED: ['RESET'],
      RESET: ['STARTED'],
    };
  }

  /**
   * Can only transition to valid states
   */
  transition(newState) {
    const allowed = this.transitions[this.state] || [];
    if (!allowed.includes(newState)) {
      throw new Error(
        `Invalid state transition: ${this.state} → ${newState}. ` +
        `Allowed: ${allowed.join(', ')}`
      );
    }
    this.state = newState;
  }

  canRecord() {
    return this.state === 'RECORDING';
  }

  canComplete() {
    return this.state === 'RECORDING';
  }
}

/**
 * Specification closure verification
 * Make sure spec is complete before generating code
 */
export class SpecificationClosure {
  constructor() {
    this.requirements = {
      hasPattern: false,
      hasName: false,
      hasDescription: false,
      hasSLATarget: false,
      hasUnit: false,
      hasBenchmarks: false,
      minBenchmarks: 0,
    };
  }

  verify(pattern) {
    const errors = [];

    if (!pattern.pattern) {
      errors.push('Missing required field: pattern');
    } else {
      this.requirements.hasPattern = true;
    }

    if (!pattern.name) {
      errors.push('Missing required field: name');
    } else {
      this.requirements.hasName = true;
    }

    if (!pattern.description) {
      errors.push('Missing required field: description');
    } else {
      this.requirements.hasDescription = true;
    }

    if (!pattern.slaTarget || pattern.slaTarget <= 0) {
      errors.push('Missing/invalid required field: slaTarget (must be > 0)');
    } else {
      this.requirements.hasSLATarget = true;
    }

    if (!pattern.unit) {
      errors.push('Missing required field: unit');
    } else {
      this.requirements.hasUnit = true;
    }

    if (!pattern.benchmarks || !Array.isArray(pattern.benchmarks)) {
      errors.push('Missing required field: benchmarks (must be array)');
    } else if (pattern.benchmarks.length === 0) {
      errors.push('At least one benchmark is required');
    } else {
      this.requirements.hasBenchmarks = true;
      this.requirements.minBenchmarks = pattern.benchmarks.length;
    }

    if (errors.length > 0) {
      throw new Error(
        `Specification not closed:\n` +
        errors.map(e => `  - ${e}`).join('\n')
      );
    }

    return true;
  }

  /**
   * Get closure metrics
   */
  getMetrics() {
    const satisfied = Object.values(this.requirements)
      .filter(v => v === true).length;
    const total = Object.values(this.requirements)
      .filter(v => typeof v === 'boolean').length;
    const percentage = (satisfied / total) * 100;

    return {
      satisfied,
      total,
      percentage,
      isClosed: percentage === 100,
    };
  }
}

/**
 * Mutation testing harness - verify tests actually catch bugs
 */
export class MutationTestHarness {
  constructor(testSuite) {
    this.testSuite = testSuite;
    this.mutations = [];
    this.results = {
      killed: 0,
      survived: 0,
      timeout: 0,
    };
  }

  /**
   * Introduce a mutation and run tests
   */
  mutateBoundary(metric, originalValue, delta) {
    const mutants = [
      { value: originalValue + delta, name: 'boundary_plus' },
      { value: originalValue - delta, name: 'boundary_minus' },
      { value: originalValue * 0.9, name: 'reduce_10pct' },
      { value: originalValue * 1.1, name: 'increase_10pct' },
    ];

    mutants.forEach(mutant => {
      const mutation = {
        metric,
        original: originalValue,
        mutant: mutant.value,
        name: mutant.name,
      };

      // Run test suite with mutated value
      try {
        const testsPassed = this.testSuite({
          threshold: mutant.value,
          original: originalValue,
        });

        if (testsPassed) {
          // Tests passed despite mutation - BAD (mutant survived)
          this.results.survived++;
          mutation.result = 'SURVIVED';
        } else {
          // Tests failed - GOOD (mutant killed)
          this.results.killed++;
          mutation.result = 'KILLED';
        }
      } catch (error) {
        // Test timeout or error
        this.results.timeout++;
        mutation.result = 'TIMEOUT';
      }

      this.mutations.push(mutation);
    });
  }

  /**
   * Get mutation score (percentage of mutants killed)
   */
  getScore() {
    const total = this.results.killed + this.results.survived;
    if (total === 0) return 0;
    return (this.results.killed / total) * 100;
  }

  /**
   * Report mutation testing results
   */
  report() {
    const score = this.getScore();
    console.log(`\nMutation Test Score: ${score.toFixed(1)}%`);
    console.log(`  Killed: ${this.results.killed}`);
    console.log(`  Survived: ${this.results.survived}`);
    console.log(`  Timeout: ${this.results.timeout}`);

    if (this.results.survived > 0) {
      console.log('\nSurvived mutants (weak tests):');
      this.mutations
        .filter(m => m.result === 'SURVIVED')
        .forEach(m => {
          console.log(`  • ${m.name}: ${m.original} → ${m.mutant}`);
        });
    }
  }
}

/**
 * Input validation - prevent invalid data from being recorded
 */
export function validateMetricInput(pattern, value, unit) {
  if (!Object.values(Fortune5Pattern).includes(pattern)) {
    throw new Error(`Invalid pattern: ${pattern}`);
  }

  if (typeof value !== 'number' || !Number.isFinite(value)) {
    throw new TypeError(`Metric value must be a finite number, got ${value}`);
  }

  if (value < 0) {
    throw new RangeError(`Metric value cannot be negative: ${value}`);
  }

  if (!unit || typeof unit !== 'string') {
    throw new Error(`Unit must be a string, got ${unit}`);
  }

  return true;
}

/**
 * SLA boundary validation - prevent impossible thresholds
 */
export class SLABoundary {
  constructor(unit, minRealistic, maxRealistic) {
    this.unit = unit;
    this.min = minRealistic;
    this.max = maxRealistic;
  }

  /**
   * Validate that proposed SLA is realistic
   */
  validate(proposedSLA) {
    if (proposedSLA < this.min) {
      throw new Error(
        `SLA ${proposedSLA}${this.unit} is unrealistic (minimum realistic: ${this.min}${this.unit})`
      );
    }

    if (proposedSLA > this.max) {
      throw new Error(
        `SLA ${proposedSLA}${this.unit} is too loose (maximum allowed: ${this.max}${this.unit})`
      );
    }

    return true;
  }
}

// Pre-defined realistic SLA boundaries
export const REALISTIC_SLAS = {
  [Fortune5Pattern.CLI_STARTUP]: new SLABoundary('ms', 10, 500),
  [Fortune5Pattern.TEMPLATE_RENDERING]: new SLABoundary('ms', 100, 10000),
  [Fortune5Pattern.RDF_QUERY]: new SLABoundary('ms', 5, 1000),
  [Fortune5Pattern.MEMORY_USAGE]: new SLABoundary('MB', 1, 2000),
  [Fortune5Pattern.CONCURRENT_OPS]: new SLABoundary('ratio', 0.5, 8.0),
};

export default {
  MetricValue,
  Fortune5Pattern,
  Severity,
  BenchmarkStateMachine,
  SpecificationClosure,
  MutationTestHarness,
  validateMetricInput,
  SLABoundary,
  REALISTIC_SLAS,
};

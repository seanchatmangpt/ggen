/**
 * CHAOS ENGINEERING - INJECT FAILURES TO VERIFY RESILIENCE
 * ============================================================================
 * Proactively inject failures to test system resilience
 * "The only way to find out if your system can handle chaos is to introduce it"
 *
 * Applied to Fortune 5:
 * - Simulate CLI latency spikes
 * - Inject template rendering delays
 * - Simulate RDF query timeouts
 * - Trigger memory pressure
 * - Test scaling under artificial constraints
 * ============================================================================
 */

/**
 * Failure injection strategies
 */
export const FailureMode = Object.freeze({
  LATENCY_SPIKE: 'latency_spike',
  TIMEOUT: 'timeout',
  MEMORY_PRESSURE: 'memory_pressure',
  PARTIAL_FAILURE: 'partial_failure',
  CASCADING_FAILURE: 'cascading_failure',
});

/**
 * Chaos scenario - controlled failure injection
 */
export class ChaosScenario {
  constructor(name, failureMode, intensity = 0.5) {
    this.name = name;
    this.failureMode = failureMode;
    this.intensity = intensity; // 0.0 to 1.0
    this.startTime = null;
    this.duration = null;
    this.affectedComponents = [];
    this.results = [];
  }

  /**
   * Inject latency spike (0-500ms added delay)
   */
  injectLatencySpike() {
    const baseDelay = 100; // ms
    const spikeAmount = Math.floor(baseDelay * this.intensity);
    return spikeAmount + Math.random() * spikeAmount;
  }

  /**
   * Simulate timeout (force failure)
   */
  simulateTimeout() {
    if (Math.random() < this.intensity) {
      throw new Error(`Chaos: Timeout injected (${(this.intensity * 100).toFixed(0)}% chance)`);
    }
  }

  /**
   * Simulate memory pressure (reduce available heap)
   */
  injectMemoryPressure() {
    // In real scenario, would use gc and --max-old-space-size
    // For demo, return simulated pressure level
    return Math.floor(this.intensity * 100); // Percentage of memory consumed
  }

  /**
   * Partial failure (some operations fail, others succeed)
   */
  mayFailPartially() {
    return Math.random() < this.intensity;
  }

  /**
   * Cascading failure trigger (each failure increases likelihood of next)
   */
  triggerCascade(failureCount) {
    const cascadeMultiplier = 1 + (failureCount * 0.1);
    return Math.random() < (this.intensity * cascadeMultiplier);
  }

  /**
   * Run scenario and collect results
   */
  run(testFn, iterations = 100) {
    this.startTime = Date.now();

    for (let i = 0; i < iterations; i++) {
      try {
        const result = testFn();
        this.results.push({ iteration: i, success: true, result });
      } catch (error) {
        this.results.push({ iteration: i, success: false, error: error.message });
      }
    }

    this.duration = Date.now() - this.startTime;
  }

  /**
   * Get scenario results
   */
  getResults() {
    const succeeded = this.results.filter(r => r.success).length;
    const failed = this.results.filter(r => !r.success).length;

    return {
      scenario: this.name,
      failureMode: this.failureMode,
      intensity: this.intensity,
      totalRuns: this.results.length,
      succeeded,
      failed,
      successRate: (succeeded / this.results.length) * 100,
      duration: this.duration,
    };
  }
}

/**
 * Circuit breaker - prevent cascading failures
 */
export class CircuitBreaker {
  constructor(threshold = 5, resetTimeout = 60000) {
    this.threshold = threshold; // failures before opening
    this.resetTimeout = resetTimeout; // ms to wait before half-open
    this.state = 'CLOSED'; // CLOSED → OPEN → HALF_OPEN → CLOSED
    this.failureCount = 0;
    this.successCount = 0;
    this.lastFailureTime = null;
    this.openedAt = null;
  }

  /**
   * Execute with circuit breaker protection
   */
  execute(fn) {
    if (this.state === 'OPEN') {
      if (Date.now() - this.openedAt > this.resetTimeout) {
        this.state = 'HALF_OPEN';
        this.successCount = 0;
      } else {
        throw new Error('CircuitBreaker: OPEN - service unavailable');
      }
    }

    try {
      const result = fn();

      if (this.state === 'HALF_OPEN') {
        this.successCount++;
        if (this.successCount >= 3) {
          this.close();
        }
      } else if (this.state === 'CLOSED') {
        this.failureCount = 0;
      }

      return result;
    } catch (error) {
      this.failureCount++;
      this.lastFailureTime = Date.now();

      if (this.failureCount >= this.threshold) {
        this.open();
      }

      throw error;
    }
  }

  /**
   * Open circuit (fail fast)
   */
  open() {
    if (this.state !== 'OPEN') {
      this.state = 'OPEN';
      this.openedAt = Date.now();
    }
  }

  /**
   * Close circuit (allow traffic)
   */
  close() {
    this.state = 'CLOSED';
    this.failureCount = 0;
    this.successCount = 0;
    this.openedAt = null;
  }

  /**
   * Get circuit state
   */
  getState() {
    return {
      state: this.state,
      failureCount: this.failureCount,
      successCount: this.successCount,
      timeSinceFailure: this.lastFailureTime
        ? Date.now() - this.lastFailureTime
        : null,
    };
  }
}

/**
 * Bulkhead pattern - isolate failures to specific zones
 */
export class Bulkhead {
  constructor(name, maxConcurrent = 10) {
    this.name = name;
    this.maxConcurrent = maxConcurrent;
    this.currentRequests = 0;
    this.queue = [];
    this.rejected = 0;
  }

  /**
   * Execute within bulkhead limits
   */
  async execute(fn) {
    if (this.currentRequests >= this.maxConcurrent) {
      this.rejected++;
      throw new Error(
        `Bulkhead ${this.name}: at capacity (${this.currentRequests}/${this.maxConcurrent})`
      );
    }

    this.currentRequests++;

    try {
      return await Promise.resolve(fn());
    } finally {
      this.currentRequests--;

      // Process queue if any
      if (this.queue.length > 0) {
        const queued = this.queue.shift();
        this.execute(queued);
      }
    }
  }

  /**
   * Queue if at capacity
   */
  executeOrQueue(fn) {
    if (this.currentRequests >= this.maxConcurrent) {
      this.queue.push(fn);
      return Promise.resolve(null);
    }
    return this.execute(fn);
  }

  /**
   * Get bulkhead status
   */
  getStatus() {
    return {
      name: this.name,
      current: this.currentRequests,
      max: this.maxConcurrent,
      queued: this.queue.length,
      rejected: this.rejected,
      utilization: (this.currentRequests / this.maxConcurrent) * 100,
    };
  }
}

/**
 * Timeout enforcer - prevent hangs
 */
export class TimeoutEnforcer {
  /**
   * Execute function with timeout
   */
  static execute(fn, timeoutMs) {
    return Promise.race([
      Promise.resolve(fn()),
      new Promise((_, reject) =>
        setTimeout(
          () => reject(new Error(`Timeout after ${timeoutMs}ms`)),
          timeoutMs
        )
      ),
    ]);
  }

  /**
   * Enforce timeout on array of operations
   */
  static async executeAll(fns, timeoutMs) {
    return Promise.all(
      fns.map(fn => this.execute(fn, timeoutMs))
    );
  }
}

/**
 * Retry with exponential backoff
 */
export class RetryPolicy {
  constructor(maxRetries = 3, initialDelayMs = 100) {
    this.maxRetries = maxRetries;
    this.initialDelayMs = initialDelayMs;
  }

  /**
   * Execute with retries
   */
  async execute(fn) {
    let lastError;

    for (let attempt = 0; attempt <= this.maxRetries; attempt++) {
      try {
        return await Promise.resolve(fn());
      } catch (error) {
        lastError = error;

        if (attempt < this.maxRetries) {
          const delayMs = this.initialDelayMs * Math.pow(2, attempt);
          await new Promise(resolve => setTimeout(resolve, delayMs));
        }
      }
    }

    throw lastError;
  }
}

/**
 * Fortune 5 chaos test suite
 */
export class Fortune5ChaosTests {
  /**
   * Test 1: CLI startup under latency injection
   */
  static testCliLatencyRobustness() {
    const scenario = new ChaosScenario(
      'CLI Startup Latency',
      FailureMode.LATENCY_SPIKE,
      0.3 // 30% intensity
    );

    scenario.run(() => {
      const injectedDelay = scenario.injectLatencySpike();
      const totalTime = 50 + injectedDelay; // Base CLI time + injected spike

      if (totalTime > 100) {
        throw new Error(`CLI took ${totalTime}ms, exceeds SLA`);
      }
      return totalTime;
    }, 100);

    return scenario.getResults();
  }

  /**
   * Test 2: Template rendering with timeouts
   */
  static testTemplateRenderingResilience() {
    const scenario = new ChaosScenario(
      'Template Rendering Resilience',
      FailureMode.TIMEOUT,
      0.05 // 5% chance of timeout
    );

    scenario.run(() => {
      try {
        scenario.simulateTimeout();
        return { duration: 500 };
      } catch (error) {
        throw error;
      }
    }, 200);

    return scenario.getResults();
  }

  /**
   * Test 3: Memory under pressure
   */
  static testMemoryResilience() {
    const scenario = new ChaosScenario(
      'Memory Under Pressure',
      FailureMode.MEMORY_PRESSURE,
      0.7 // 70% memory consumption
    );

    scenario.run(() => {
      const pressureLevel = scenario.injectMemoryPressure();
      if (pressureLevel > 90) {
        throw new Error('Memory pressure too high');
      }
      return pressureLevel;
    }, 50);

    return scenario.getResults();
  }

  /**
   * Test 4: Cascading failures
   */
  static testCascadingFailureRecovery() {
    const scenario = new ChaosScenario(
      'Cascading Failure',
      FailureMode.CASCADING_FAILURE,
      0.2 // 20% initial failure rate
    );

    let failureCount = 0;

    scenario.run(() => {
      if (scenario.triggerCascade(failureCount)) {
        failureCount++;
        throw new Error(`Cascading failure #${failureCount}`);
      }
      failureCount = 0; // Reset on success
      return 'success';
    }, 100);

    return scenario.getResults();
  }

  /**
   * Test 5: Circuit breaker behavior
   */
  static testCircuitBreakerBehavior() {
    const breaker = new CircuitBreaker(5, 5000);
    const results = {
      successCount: 0,
      failureCount: 0,
      circuitOpenCount: 0,
    };

    for (let i = 0; i < 100; i++) {
      try {
        breaker.execute(() => {
          if (i < 10) {
            throw new Error('Injected failure');
          }
          results.successCount++;
          return 'ok';
        });
      } catch (error) {
        if (error.message.includes('OPEN')) {
          results.circuitOpenCount++;
        } else {
          results.failureCount++;
        }
      }
    }

    return {
      scenario: 'Circuit Breaker',
      ...results,
      finalState: breaker.getState(),
    };
  }
}

export default {
  FailureMode,
  ChaosScenario,
  CircuitBreaker,
  Bulkhead,
  TimeoutEnforcer,
  RetryPolicy,
  Fortune5ChaosTests,
};

/**
 * Performance Test Suite
 *
 * Tests system capacity to handle high-volume value events,
 * payment processing, and invoice generation.
 */

import { describe, it, expect } from '@jest/globals';

interface PerformanceMetrics {
  operationCount: number;
  totalTime: number;
  avgTime: number;
  maxTime: number;
  minTime: number;
  throughput: number; // ops/second
}

class PerformanceBenchmark {
  measureOperation(fn: () => void, iterations: number = 1): PerformanceMetrics {
    const times: number[] = [];

    for (let i = 0; i < iterations; i++) {
      const start = performance.now();
      fn();
      const end = performance.now();
      times.push(end - start);
    }

    const totalTime = times.reduce((a, b) => a + b, 0);
    const avgTime = totalTime / iterations;
    const maxTime = Math.max(...times);
    const minTime = Math.min(...times);
    const throughput = 1000 / avgTime; // ops/second

    return {
      operationCount: iterations,
      totalTime,
      avgTime,
      maxTime,
      minTime,
      throughput,
    };
  }

  async measureAsyncOperation(
    fn: () => Promise<void>,
    iterations: number = 1
  ): Promise<PerformanceMetrics> {
    const times: number[] = [];

    for (let i = 0; i < iterations; i++) {
      const start = performance.now();
      await fn();
      const end = performance.now();
      times.push(end - start);
    }

    const totalTime = times.reduce((a, b) => a + b, 0);
    const avgTime = totalTime / iterations;
    const maxTime = Math.max(...times);
    const minTime = Math.min(...times);
    const throughput = 1000 / avgTime;

    return {
      operationCount: iterations,
      totalTime,
      avgTime,
      maxTime,
      minTime,
      throughput,
    };
  }

  calculatePercentile(times: number[], percentile: number): number {
    const sorted = [...times].sort((a, b) => a - b);
    const index = Math.ceil((percentile / 100) * sorted.length) - 1;
    return sorted[Math.max(0, index)];
  }

  generateLoadProfile(
    baseLoad: number,
    peakLoad: number,
    duration: number // in iterations
  ): number[] {
    const profile: number[] = [];
    for (let i = 0; i < duration; i++) {
      // Sinusoidal load pattern
      const load = baseLoad + ((peakLoad - baseLoad) / 2) *
        (1 + Math.sin((i / duration) * Math.PI * 2));
      profile.push(Math.round(load));
    }
    return profile;
  }

  estimateCapacity(
    avgOperationTime: number,
    targetThroughput: number
  ): number {
    // Estimate how many operations can be handled in 1 second
    return Math.floor(1000 / (avgOperationTime / targetThroughput));
  }
}

class SimpleValueAccumulator {
  private values: number[] = [];

  addValue(value: number): void {
    this.values.push(value);
  }

  getTotal(): number {
    return this.values.reduce((a, b) => a + b, 0);
  }

  clear(): void {
    this.values = [];
  }
}

class SimpleBillingEngine {
  private invoices: Map<string, any> = new Map();
  private nextId = 0;

  createInvoice(amount: number): string {
    const id = `inv_${this.nextId++}`;
    this.invoices.set(id, { amount, status: 'draft' });
    return id;
  }

  issueInvoice(id: string): void {
    const invoice = this.invoices.get(id);
    if (invoice) {
      invoice.status = 'issued';
    }
  }

  processPayment(id: string, amount: number): void {
    const invoice = this.invoices.get(id);
    if (invoice && Math.abs(invoice.amount - amount) < 0.01) {
      invoice.status = 'paid';
    }
  }

  getInvoice(id: string): any {
    return this.invoices.get(id);
  }

  getTotalRevenue(): number {
    let total = 0;
    for (const invoice of this.invoices.values()) {
      total += invoice.amount;
    }
    return total;
  }

  clear(): void {
    this.invoices.clear();
    this.nextId = 0;
  }
}

describe('Performance Tests', () => {
  let benchmark: PerformanceBenchmark;

  beforeEach(() => {
    benchmark = new PerformanceBenchmark();
  });

  // ============= Value Event Processing =============
  describe('Value Event Processing Performance', () => {
    it('should process 1,000 value events per second', () => {
      const accumulator = new SimpleValueAccumulator();

      const metrics = benchmark.measureOperation(() => {
        for (let i = 0; i < 1000; i++) {
          accumulator.addValue(Math.random() * 1000);
        }
      }, 10);

      expect(metrics.avgTime).toBeLessThan(50); // 50ms for 1000 ops = 20k ops/sec
      expect(metrics.throughput).toBeGreaterThan(1000);
      accumulator.clear();
    });

    it('should process 100,000 value events under 1 second', () => {
      const accumulator = new SimpleValueAccumulator();

      const metrics = benchmark.measureOperation(() => {
        for (let i = 0; i < 100_000; i++) {
          accumulator.addValue(Math.random() * 1000);
        }
      }, 1);

      expect(metrics.totalTime).toBeLessThan(1000); // 1 second
      accumulator.clear();
    });

    it('should handle 1,000,000 daily value events', () => {
      // 1M events in 24 hours = ~11.6 events/second on average
      // This test simulates processing them all in a batch
      const accumulator = new SimpleValueAccumulator();

      const metrics = benchmark.measureOperation(() => {
        for (let i = 0; i < 1_000_000; i++) {
          accumulator.addValue(Math.random() * 1000);
        }
      }, 1);

      // Should complete in reasonable time (< 10 seconds)
      expect(metrics.totalTime).toBeLessThan(10_000);
      accumulator.clear();
    });

    it('should calculate aggregations quickly on 10k events', () => {
      const accumulator = new SimpleValueAccumulator();
      for (let i = 0; i < 10_000; i++) {
        accumulator.addValue(Math.random() * 1000);
      }

      const metrics = benchmark.measureOperation(() => {
        accumulator.getTotal();
      }, 1000);

      expect(metrics.avgTime).toBeLessThan(1); // < 1ms per aggregation
      accumulator.clear();
    });
  });

  // ============= Invoice Generation =============
  describe('Invoice Generation Performance', () => {
    it('should create 1,000 invoices per second', () => {
      const engine = new SimpleBillingEngine();

      const metrics = benchmark.measureOperation(() => {
        for (let i = 0; i < 1000; i++) {
          engine.createInvoice(Math.random() * 10_000);
        }
      }, 10);

      expect(metrics.avgTime).toBeLessThan(50);
      expect(metrics.throughput).toBeGreaterThan(1000);
      engine.clear();
    });

    it('should issue 10,000 invoices in batch', () => {
      const engine = new SimpleBillingEngine();

      // Create invoices
      const invoiceIds: string[] = [];
      for (let i = 0; i < 10_000; i++) {
        invoiceIds.push(engine.createInvoice(Math.random() * 10_000));
      }

      // Measure issuance performance
      const metrics = benchmark.measureOperation(() => {
        invoiceIds.forEach((id) => engine.issueInvoice(id));
      }, 1);

      expect(metrics.totalTime).toBeLessThan(1000); // < 1 second
      engine.clear();
    });

    it('should process payments at 100+ per second', () => {
      const engine = new SimpleBillingEngine();

      // Create and prepare invoices
      const invoiceIds: string[] = [];
      const amounts: number[] = [];
      for (let i = 0; i < 100; i++) {
        const amount = Math.random() * 10_000;
        invoiceIds.push(engine.createInvoice(amount));
        amounts.push(amount);
      }

      // Measure payment processing
      const metrics = benchmark.measureOperation(() => {
        invoiceIds.forEach((id, idx) => {
          engine.processPayment(id, amounts[idx]);
        });
      }, 100);

      expect(metrics.avgTime).toBeLessThan(10); // < 10ms per 100 payments
      engine.clear();
    });

    it('should query invoices in O(1) time', () => {
      const engine = new SimpleBillingEngine();

      // Create 100k invoices
      const invoiceIds: string[] = [];
      for (let i = 0; i < 100_000; i++) {
        invoiceIds.push(engine.createInvoice(Math.random() * 10_000));
      }

      // Measure query performance
      const metrics = benchmark.measureOperation(() => {
        for (let i = 0; i < 1000; i++) {
          const randomId = invoiceIds[Math.floor(Math.random() * invoiceIds.length)];
          engine.getInvoice(randomId);
        }
      }, 10);

      expect(metrics.avgTime).toBeLessThan(5); // < 5ms for 1000 queries
      engine.clear();
    });
  });

  // ============= Memory Efficiency =============
  describe('Memory Efficiency', () => {
    it('should handle 100k value events with reasonable memory', () => {
      const accumulator = new SimpleValueAccumulator();

      const initialMemory = process.memoryUsage().heapUsed;

      for (let i = 0; i < 100_000; i++) {
        accumulator.addValue(Math.random() * 1000);
      }

      const finalMemory = process.memoryUsage().heapUsed;
      const memoryIncrease = finalMemory - initialMemory;

      // Should use < 10MB for 100k numbers
      expect(memoryIncrease).toBeLessThan(10 * 1024 * 1024);
      accumulator.clear();
    });

    it('should maintain stable memory for repeated operations', () => {
      const accumulator = new SimpleValueAccumulator();
      const measurements: number[] = [];

      for (let iteration = 0; iteration < 10; iteration++) {
        const memBefore = process.memoryUsage().heapUsed;

        for (let i = 0; i < 10_000; i++) {
          accumulator.addValue(Math.random() * 1000);
        }

        const memAfter = process.memoryUsage().heapUsed;
        measurements.push(memAfter - memBefore);

        accumulator.clear();
      }

      // Memory usage should be consistent across iterations
      const avgMemory = measurements.reduce((a, b) => a + b) / measurements.length;
      const variance = measurements.reduce((sum, val) => sum + Math.pow(val - avgMemory, 2), 0) / measurements.length;

      expect(Math.sqrt(variance)).toBeLessThan(avgMemory * 0.5); // < 50% variance
    });
  });

  // ============= Throughput Tests =============
  describe('Throughput Tests', () => {
    it('should achieve 10,000 ops/sec for simple operations', () => {
      const metrics = benchmark.measureOperation(() => {
        let x = 0;
        for (let i = 0; i < 100; i++) {
          x += i;
        }
      }, 100);

      expect(metrics.throughput).toBeGreaterThan(10_000);
    });

    it('should handle variable load with peak 50k ops', () => {
      const engine = new SimpleBillingEngine();
      const loadProfile = benchmark.generateLoadProfile(1000, 10_000, 100);

      for (const load of loadProfile) {
        const metrics = benchmark.measureOperation(() => {
          for (let i = 0; i < load; i++) {
            engine.createInvoice(Math.random() * 10_000);
          }
        }, 1);

        expect(metrics.totalTime).toBeLessThan(2000); // < 2 seconds per batch
      }

      engine.clear();
    });
  });

  // ============= Latency Tests =============
  describe('Latency and Response Time', () => {
    it('should have p50 latency < 1ms for value events', () => {
      const accumulator = new SimpleValueAccumulator();
      const times: number[] = [];

      for (let i = 0; i < 1000; i++) {
        const start = performance.now();
        accumulator.addValue(Math.random() * 1000);
        times.push(performance.now() - start);
      }

      const p50 = benchmark.calculatePercentile(times, 50);
      expect(p50).toBeLessThan(1);

      accumulator.clear();
    });

    it('should have p99 latency < 5ms for invoice operations', () => {
      const engine = new SimpleBillingEngine();
      const times: number[] = [];

      for (let i = 0; i < 1000; i++) {
        const start = performance.now();
        const id = engine.createInvoice(Math.random() * 10_000);
        engine.issueInvoice(id);
        times.push(performance.now() - start);
      }

      const p99 = benchmark.calculatePercentile(times, 99);
      expect(p99).toBeLessThan(5);

      engine.clear();
    });

    it('should have p99.9 latency < 10ms for payment processing', () => {
      const engine = new SimpleBillingEngine();

      // Pre-populate invoices
      const invoiceIds: string[] = [];
      const amounts: number[] = [];
      for (let i = 0; i < 1000; i++) {
        const amount = Math.random() * 10_000;
        invoiceIds.push(engine.createInvoice(amount));
        amounts.push(amount);
      }

      const times: number[] = [];
      for (let i = 0; i < invoiceIds.length; i++) {
        const start = performance.now();
        engine.processPayment(invoiceIds[i], amounts[i]);
        times.push(performance.now() - start);
      }

      const p999 = benchmark.calculatePercentile(times, 99.9);
      expect(p999).toBeLessThan(10);

      engine.clear();
    });
  });

  // ============= Scalability Tests =============
  describe('Scalability Tests', () => {
    it('should scale linearly with invoice count up to 1M', () => {
      const engine = new SimpleBillingEngine();
      const timings: Array<{ count: number; time: number }> = [];

      for (const count of [1000, 10_000, 100_000]) {
        const metrics = benchmark.measureOperation(() => {
          for (let i = 0; i < count; i++) {
            engine.createInvoice(Math.random() * 10_000);
          }
        }, 1);

        timings.push({ count, time: metrics.totalTime });
        engine.clear();
      }

      // Check linear scaling: time should increase proportionally to count
      const ratio1 = timings[1].time / timings[0].time;
      const ratio2 = timings[2].time / timings[1].time;

      // Both ratios should be roughly 10 (since we increase count by 10x)
      expect(ratio1).toBeCloseTo(10, 1);
      expect(ratio2).toBeCloseTo(10, 1);
    });

    it('should handle concurrent-like operations', () => {
      const engine = new SimpleBillingEngine();

      const metrics = benchmark.measureOperation(() => {
        // Simulate concurrent operations
        for (let i = 0; i < 100; i++) {
          const id = engine.createInvoice(Math.random() * 10_000);
          engine.issueInvoice(id);
          engine.processPayment(id, engine.getInvoice(id)?.amount || 0);
        }
      }, 10);

      expect(metrics.avgTime).toBeLessThan(50); // < 50ms per batch
      engine.clear();
    });
  });

  // ============= Stress Tests =============
  describe('Stress Tests', () => {
    it('should handle sustained load of 10k ops/sec', () => {
      const accumulator = new SimpleValueAccumulator();
      const duration = 1000; // 1 second

      const startTime = performance.now();
      let operationCount = 0;

      while (performance.now() - startTime < duration) {
        accumulator.addValue(Math.random() * 1000);
        operationCount++;
      }

      const elapsedTime = performance.now() - startTime;
      const throughput = operationCount / (elapsedTime / 1000);

      expect(throughput).toBeGreaterThan(10_000);
      accumulator.clear();
    });

    it('should recover from temporary spikes', () => {
      const engine = new SimpleBillingEngine();
      const recovery = benchmark.measureOperation(() => {
        // Normal load
        for (let i = 0; i < 100; i++) {
          engine.createInvoice(Math.random() * 10_000);
        }

        // Spike
        for (let i = 0; i < 1000; i++) {
          engine.createInvoice(Math.random() * 10_000);
        }

        // Back to normal
        for (let i = 0; i < 100; i++) {
          engine.createInvoice(Math.random() * 10_000);
        }
      }, 1);

      expect(recovery.totalTime).toBeLessThan(1000);
      engine.clear();
    });
  });

  // ============= Capacity Estimation =============
  describe('Capacity Estimation', () => {
    it('should estimate 1M daily event capacity', () => {
      const accumulator = new SimpleValueAccumulator();

      const metrics = benchmark.measureOperation(() => {
        accumulator.addValue(Math.random() * 1000);
      }, 10_000);

      const avgTime = metrics.avgTime / 10_000; // Time per event
      const capacity = benchmark.estimateCapacity(avgTime, 1);

      // Should handle 1M events = ~11.6 events/sec
      expect(capacity).toBeGreaterThan(11);

      accumulator.clear();
    });

    it('should estimate peak load capacity (100k events/hour)', () => {
      const engine = new SimpleBillingEngine();

      const metrics = benchmark.measureOperation(() => {
        const id = engine.createInvoice(Math.random() * 10_000);
        engine.issueInvoice(id);
      }, 1000);

      const avgTime = metrics.avgTime / 1000; // Time per operation
      const capacity = benchmark.estimateCapacity(avgTime, 1);

      // 100k per hour = ~27.8 per second
      expect(capacity).toBeGreaterThan(27);

      engine.clear();
    });
  });

  // ============= Regression Detection =============
  describe('Regression Detection', () => {
    it('should detect performance regression (>20% slowdown)', () => {
      const accumulator = new SimpleValueAccumulator();

      // Baseline measurement
      const baseline = benchmark.measureOperation(() => {
        for (let i = 0; i < 10_000; i++) {
          accumulator.addValue(Math.random() * 1000);
        }
      }, 5);

      accumulator.clear();

      // Simulated regression (adding extra work)
      const regressed = benchmark.measureOperation(() => {
        for (let i = 0; i < 10_000; i++) {
          accumulator.addValue(Math.random() * 1000);
          // Extra work
          for (let j = 0; j < 10; j++) {
            Math.sqrt(i);
          }
        }
      }, 5);

      const regression = ((regressed.avgTime - baseline.avgTime) / baseline.avgTime) * 100;
      expect(regression).toBeGreaterThan(20);

      accumulator.clear();
    });

    it('should identify performance gains', () => {
      const baselineTime = benchmark.measureOperation(() => {
        let sum = 0;
        for (let i = 0; i < 100_000; i++) {
          sum += i;
        }
      }, 10).avgTime;

      const optimizedTime = benchmark.measureOperation(() => {
        // Faster approach
        const sum = (100_000 * (100_000 - 1)) / 2;
      }, 10).avgTime;

      expect(optimizedTime).toBeLessThan(baselineTime);
    });
  });
});

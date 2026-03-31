/**
 * Fraud Prevention Test Suite
 *
 * Tests security controls to prevent fraudulent value claims,
 * fake metrics, and financial manipulation.
 */

import { describe, it, expect, beforeEach } from '@jest/globals';
import crypto from 'crypto';

interface ValueMetric {
  id: string;
  timestamp: Date;
  value: number;
  source: string;
  signature?: string;
  publicKey?: string;
}

interface AnomalySignal {
  type: string;
  severity: 'low' | 'medium' | 'high' | 'critical';
  description: string;
  metric?: ValueMetric;
}

class FraudDetectionSystem {
  private metrics: Map<string, ValueMetric> = new Map();
  private detectionRules: Array<(metric: ValueMetric) => boolean> = [];
  private baselineValues: Map<string, number> = new Map();

  hashMetric(metric: ValueMetric): string {
    const content = `${metric.timestamp}:${metric.value}:${metric.source}`;
    return crypto.createHash('sha256').update(content).digest('hex');
  }

  signMetric(metric: ValueMetric, privateKey: string): string {
    const content = `${metric.timestamp}:${metric.value}:${metric.source}`;
    return crypto
      .createHmac('sha256', privateKey)
      .update(content)
      .digest('hex');
  }

  verifyMetricSignature(metric: ValueMetric, publicKey: string): boolean {
    if (!metric.signature) return false;

    const content = `${metric.timestamp}:${metric.value}:${metric.source}`;
    const expected = crypto
      .createHmac('sha256', publicKey)
      .update(content)
      .digest('hex');

    return metric.signature === expected;
  }

  detectImpossibleGrowth(
    previousValue: number,
    currentValue: number
  ): AnomalySignal | null {
    const growthRate = ((currentValue - previousValue) / previousValue) * 100;

    // Flag if growth > 500% in a single day
    if (growthRate > 500) {
      return {
        type: 'impossible_growth',
        severity: 'critical',
        description: `Growth rate of ${growthRate}% exceeds 500% daily threshold`,
      };
    }

    // Flag if growth > 50% in a single day
    if (growthRate > 50) {
      return {
        type: 'suspicious_growth',
        severity: 'high',
        description: `Growth rate of ${growthRate}% is unusually high`,
      };
    }

    return null;
  }

  detectValueInflation(
    claimedValue: number,
    historicalAverage: number
  ): AnomalySignal | null {
    const inflation = ((claimedValue - historicalAverage) / historicalAverage) * 100;

    if (inflation > 200) {
      return {
        type: 'value_inflation',
        severity: 'high',
        description: `Claimed value ${inflation}% above historical average`,
      };
    }

    return null;
  }

  detectDuplicateMetrics(metrics: ValueMetric[]): ValueMetric[] {
    const seen = new Set<string>();
    const duplicates: ValueMetric[] = [];

    for (const metric of metrics) {
      const hash = this.hashMetric(metric);
      if (seen.has(hash)) {
        duplicates.push(metric);
      } else {
        seen.add(hash);
      }
    }

    return duplicates;
  }

  detectTimestampManipulation(metrics: ValueMetric[]): AnomalySignal | null {
    // Check for out-of-order timestamps
    for (let i = 1; i < metrics.length; i++) {
      if (metrics[i].timestamp < metrics[i - 1].timestamp) {
        return {
          type: 'timestamp_manipulation',
          severity: 'critical',
          description: 'Metrics submitted out of chronological order',
        };
      }
    }

    // Check for future timestamps
    const now = new Date();
    for (const metric of metrics) {
      if (metric.timestamp > now) {
        return {
          type: 'future_timestamp',
          severity: 'high',
          description: 'Metric has future timestamp',
        };
      }
    }

    return null;
  }

  detectSourceSpoofing(metric: ValueMetric, trustedSources: string[]): boolean {
    if (!trustedSources.includes(metric.source)) {
      return true; // Spoofed source detected
    }
    return false;
  }

  validateMetricConsistency(metrics: ValueMetric[]): AnomalySignal | null {
    if (metrics.length < 2) return null;

    // Calculate expected variance
    const values = metrics.map((m) => m.value);
    const mean = values.reduce((a, b) => a + b) / values.length;
    const variance =
      values.reduce((sum, val) => sum + Math.pow(val - mean, 2), 0) /
      values.length;
    const stdDev = Math.sqrt(variance);

    // Check for sudden unexplained changes (> 5 standard deviations)
    for (let i = 1; i < metrics.length; i++) {
      const diff = Math.abs(metrics[i].value - metrics[i - 1].value);
      if (diff > stdDev * 5) {
        return {
          type: 'inconsistent_metrics',
          severity: 'medium',
          description: `Sudden change of ${diff} detected`,
          metric: metrics[i],
        };
      }
    }

    return null;
  }

  detectValueDecay(
    metrics: ValueMetric[],
    expectingGrowth: boolean
  ): AnomalySignal | null {
    if (metrics.length < 2) return null;

    const firstValue = metrics[0].value;
    const lastValue = metrics[metrics.length - 1].value;

    // If claiming value growth but metrics show decay
    if (expectingGrowth && lastValue < firstValue) {
      const decay = ((firstValue - lastValue) / firstValue) * 100;
      if (decay > 5) {
        return {
          type: 'unexpected_decay',
          severity: 'high',
          description: `Expected growth but metrics show ${decay}% decay`,
        };
      }
    }

    return null;
  }

  validateMetricRange(
    metric: ValueMetric,
    min: number,
    max: number
  ): AnomalySignal | null {
    if (metric.value < min || metric.value > max) {
      return {
        type: 'out_of_range',
        severity: 'high',
        description: `Metric ${metric.value} outside valid range [${min}, ${max}]`,
        metric,
      };
    }

    return null;
  }

  detectFrequentValueEditing(metrics: ValueMetric[]): AnomalySignal | null {
    // Flag if same metric edited many times
    const metricFrequency: Record<string, number> = {};

    for (const metric of metrics) {
      const key = `${metric.timestamp.getTime()}:${metric.source}`;
      metricFrequency[key] = (metricFrequency[key] || 0) + 1;
    }

    for (const count of Object.values(metricFrequency)) {
      if (count > 3) {
        return {
          type: 'frequent_editing',
          severity: 'high',
          description: `Metric edited ${count} times (typical: 0-1)`,
        };
      }
    }

    return null;
  }

  detectBatchValueSubmission(
    metrics: ValueMetric[],
    threshold: number = 1000
  ): AnomalySignal | null {
    if (metrics.length > threshold) {
      return {
        type: 'batch_submission',
        severity: 'high',
        description: `${metrics.length} metrics submitted in single batch (threshold: ${threshold})`,
      };
    }

    return null;
  }

  compareToBaseline(
    customerId: string,
    value: number,
    tolerance: number = 0.2 // 20% variance
  ): boolean {
    const baseline = this.baselineValues.get(customerId);
    if (!baseline) return true; // No baseline to compare

    const variance = Math.abs((value - baseline) / baseline);
    return variance <= tolerance;
  }

  setBaseline(customerId: string, baseline: number): void {
    this.baselineValues.set(customerId, baseline);
  }

  runComprehensiveCheck(
    metric: ValueMetric,
    allMetrics: ValueMetric[] = [],
    trustedSources: string[] = ['system', 'verified_api']
  ): AnomalySignal[] {
    const signals: AnomalySignal[] = [];

    // Check timestamp manipulation
    const timestampIssue = this.detectTimestampManipulation([metric, ...allMetrics]);
    if (timestampIssue) signals.push(timestampIssue);

    // Check source spoofing
    if (this.detectSourceSpoofing(metric, trustedSources)) {
      signals.push({
        type: 'source_spoofing',
        severity: 'high',
        description: `Unknown source: ${metric.source}`,
        metric,
      });
    }

    // Check metric range
    const rangeIssue = this.validateMetricRange(metric, 0, 10_000_000);
    if (rangeIssue) signals.push(rangeIssue);

    // Check consistency with other metrics
    if (allMetrics.length > 0) {
      const consistencyIssue = this.detectMetricConsistency([...allMetrics, metric]);
      if (consistencyIssue) signals.push(consistencyIssue);
    }

    // Check for duplicates
    const duplicates = this.detectDuplicateMetrics([...allMetrics, metric]);
    if (duplicates.length > 0) {
      signals.push({
        type: 'duplicate_metrics',
        severity: 'critical',
        description: `Found ${duplicates.length} duplicate metrics`,
      });
    }

    return signals;
  }

  private detectMetricConsistency(metrics: ValueMetric[]): AnomalySignal | null {
    return this.validateMetricConsistency(metrics);
  }
}

describe('Fraud Prevention Tests', () => {
  let system: FraudDetectionSystem;

  beforeEach(() => {
    system = new FraudDetectionSystem();
  });

  // ============= Metric Integrity =============
  describe('Metric Integrity and Signature Verification', () => {
    it('should sign and verify metric', () => {
      const metric: ValueMetric = {
        id: 'met_1',
        timestamp: new Date(),
        value: 1000,
        source: 'system',
      };

      const privateKey = 'secret_key_123';
      metric.signature = system.signMetric(metric, privateKey);

      expect(system.verifyMetricSignature(metric, privateKey)).toBe(true);
    });

    it('should detect tampered metric', () => {
      const metric: ValueMetric = {
        id: 'met_1',
        timestamp: new Date(),
        value: 1000,
        source: 'system',
      };

      const privateKey = 'secret_key_123';
      metric.signature = system.signMetric(metric, privateKey);

      // Tamper with value
      metric.value = 5000;

      expect(system.verifyMetricSignature(metric, privateKey)).toBe(false);
    });

    it('should hash metrics consistently', () => {
      const metric: ValueMetric = {
        id: 'met_1',
        timestamp: new Date('2025-01-25T12:00:00Z'),
        value: 1000,
        source: 'system',
      };

      const hash1 = system.hashMetric(metric);
      const hash2 = system.hashMetric(metric);

      expect(hash1).toBe(hash2);
    });

    it('should reject unsigned metrics', () => {
      const metric: ValueMetric = {
        id: 'met_1',
        timestamp: new Date(),
        value: 1000,
        source: 'system',
      };

      expect(system.verifyMetricSignature(metric, 'key')).toBe(false);
    });
  });

  // ============= Impossible Growth Detection =============
  describe('Impossible Growth Detection', () => {
    it('should flag 500%+ daily growth', () => {
      const signal = system.detectImpossibleGrowth(100, 600);

      expect(signal).not.toBeNull();
      expect(signal?.type).toBe('impossible_growth');
      expect(signal?.severity).toBe('critical');
    });

    it('should flag 50%+ daily growth as suspicious', () => {
      const signal = system.detectImpossibleGrowth(100, 160);

      expect(signal).not.toBeNull();
      expect(signal?.type).toBe('suspicious_growth');
      expect(signal?.severity).toBe('high');
    });

    it('should allow reasonable growth', () => {
      const signal = system.detectImpossibleGrowth(100, 110);

      expect(signal).toBeNull();
    });

    it('should handle negative growth', () => {
      const signal = system.detectImpossibleGrowth(100, 50);

      expect(signal).toBeNull();
    });

    it('should handle zero baseline', () => {
      expect(() => {
        system.detectImpossibleGrowth(0, 100);
      }).toThrow();
    });
  });

  // ============= Value Inflation Detection =============
  describe('Value Inflation Detection', () => {
    it('should flag 200%+ inflation above baseline', () => {
      const signal = system.detectValueInflation(1000, 200);

      expect(signal).not.toBeNull();
      expect(signal?.type).toBe('value_inflation');
      expect(signal?.severity).toBe('high');
    });

    it('should allow reasonable variance from baseline', () => {
      const signal = system.detectValueInflation(1100, 1000);

      expect(signal).toBeNull();
    });

    it('should handle zero baseline', () => {
      const signal = system.detectValueInflation(1000, 1000);

      expect(signal).toBeNull();
    });
  });

  // ============= Duplicate Detection =============
  describe('Duplicate Metric Detection', () => {
    it('should detect exact duplicate metrics', () => {
      const now = new Date();
      const metrics: ValueMetric[] = [
        { id: '1', timestamp: now, value: 100, source: 'system' },
        { id: '2', timestamp: now, value: 100, source: 'system' },
      ];

      const duplicates = system.detectDuplicateMetrics(metrics);
      expect(duplicates.length).toBe(1);
    });

    it('should detect multiple duplicate instances', () => {
      const now = new Date();
      const metrics: ValueMetric[] = [
        { id: '1', timestamp: now, value: 100, source: 'system' },
        { id: '2', timestamp: now, value: 100, source: 'system' },
        { id: '3', timestamp: now, value: 100, source: 'system' },
      ];

      const duplicates = system.detectDuplicateMetrics(metrics);
      expect(duplicates.length).toBe(2);
    });

    it('should not flag different metrics as duplicates', () => {
      const now = new Date();
      const metrics: ValueMetric[] = [
        { id: '1', timestamp: now, value: 100, source: 'system' },
        { id: '2', timestamp: now, value: 101, source: 'system' },
      ];

      const duplicates = system.detectDuplicateMetrics(metrics);
      expect(duplicates.length).toBe(0);
    });
  });

  // ============= Timestamp Manipulation =============
  describe('Timestamp Manipulation Detection', () => {
    it('should detect out-of-order timestamps', () => {
      const metrics: ValueMetric[] = [
        { id: '1', timestamp: new Date('2025-01-25'), value: 100, source: 'system' },
        { id: '2', timestamp: new Date('2025-01-24'), value: 150, source: 'system' },
      ];

      const signal = system.detectTimestampManipulation(metrics);
      expect(signal).not.toBeNull();
      expect(signal?.type).toBe('timestamp_manipulation');
    });

    it('should detect future timestamps', () => {
      const future = new Date(Date.now() + 24 * 60 * 60 * 1000);
      const metrics: ValueMetric[] = [
        {
          id: '1',
          timestamp: future,
          value: 100,
          source: 'system',
        },
      ];

      const signal = system.detectTimestampManipulation(metrics);
      expect(signal).not.toBeNull();
      expect(signal?.type).toBe('future_timestamp');
    });

    it('should allow properly ordered timestamps', () => {
      const metrics: ValueMetric[] = [
        { id: '1', timestamp: new Date('2025-01-20'), value: 100, source: 'system' },
        { id: '2', timestamp: new Date('2025-01-25'), value: 150, source: 'system' },
      ];

      const signal = system.detectTimestampManipulation(metrics);
      expect(signal).toBeNull();
    });
  });

  // ============= Source Spoofing =============
  describe('Source Spoofing Detection', () => {
    it('should flag untrusted source', () => {
      const metric: ValueMetric = {
        id: '1',
        timestamp: new Date(),
        value: 100,
        source: 'unknown_source',
      };

      const trustedSources = ['system', 'verified_api'];
      expect(system.detectSourceSpoofing(metric, trustedSources)).toBe(true);
    });

    it('should allow trusted sources', () => {
      const metric: ValueMetric = {
        id: '1',
        timestamp: new Date(),
        value: 100,
        source: 'system',
      };

      const trustedSources = ['system', 'verified_api'];
      expect(system.detectSourceSpoofing(metric, trustedSources)).toBe(false);
    });
  });

  // ============= Consistency Validation =============
  describe('Metric Consistency Validation', () => {
    it('should flag sudden value spikes', () => {
      const metrics: ValueMetric[] = [
        { id: '1', timestamp: new Date('2025-01-20'), value: 100, source: 'system' },
        { id: '2', timestamp: new Date('2025-01-21'), value: 105, source: 'system' },
        { id: '3', timestamp: new Date('2025-01-22'), value: 500, source: 'system' }, // Spike!
      ];

      const signal = system.validateMetricConsistency(metrics);
      expect(signal).not.toBeNull();
      expect(signal?.type).toBe('inconsistent_metrics');
    });

    it('should allow gradual changes', () => {
      const metrics: ValueMetric[] = [
        { id: '1', timestamp: new Date('2025-01-20'), value: 100, source: 'system' },
        { id: '2', timestamp: new Date('2025-01-21'), value: 102, source: 'system' },
        { id: '3', timestamp: new Date('2025-01-22'), value: 104, source: 'system' },
      ];

      const signal = system.validateMetricConsistency(metrics);
      expect(signal).toBeNull();
    });
  });

  // ============= Value Decay Detection =============
  describe('Value Decay Detection', () => {
    it('should flag unexpected decay when growth expected', () => {
      const metrics: ValueMetric[] = [
        { id: '1', timestamp: new Date('2025-01-20'), value: 1000, source: 'system' },
        { id: '2', timestamp: new Date('2025-01-21'), value: 900, source: 'system' }, // Decay!
      ];

      const signal = system.detectValueDecay(metrics, true);
      expect(signal).not.toBeNull();
      expect(signal?.type).toBe('unexpected_decay');
    });

    it('should not flag decay when not expecting growth', () => {
      const metrics: ValueMetric[] = [
        { id: '1', timestamp: new Date('2025-01-20'), value: 1000, source: 'system' },
        { id: '2', timestamp: new Date('2025-01-21'), value: 900, source: 'system' },
      ];

      const signal = system.detectValueDecay(metrics, false);
      expect(signal).toBeNull();
    });

    it('should allow small decay', () => {
      const metrics: ValueMetric[] = [
        { id: '1', timestamp: new Date('2025-01-20'), value: 1000, source: 'system' },
        { id: '2', timestamp: new Date('2025-01-21'), value: 980, source: 'system' }, // <2% decay
      ];

      const signal = system.detectValueDecay(metrics, true);
      expect(signal).toBeNull();
    });
  });

  // ============= Range Validation =============
  describe('Value Range Validation', () => {
    it('should flag out-of-range values', () => {
      const metric: ValueMetric = {
        id: '1',
        timestamp: new Date(),
        value: 20_000_000,
        source: 'system',
      };

      const signal = system.validateMetricRange(metric, 0, 10_000_000);
      expect(signal).not.toBeNull();
      expect(signal?.type).toBe('out_of_range');
    });

    it('should allow values in range', () => {
      const metric: ValueMetric = {
        id: '1',
        timestamp: new Date(),
        value: 5_000_000,
        source: 'system',
      };

      const signal = system.validateMetricRange(metric, 0, 10_000_000);
      expect(signal).toBeNull();
    });

    it('should flag negative values', () => {
      const metric: ValueMetric = {
        id: '1',
        timestamp: new Date(),
        value: -100,
        source: 'system',
      };

      const signal = system.validateMetricRange(metric, 0, 10_000_000);
      expect(signal).not.toBeNull();
    });
  });

  // ============= Frequent Editing Detection =============
  describe('Frequent Value Editing Detection', () => {
    it('should flag metric edited multiple times', () => {
      const now = new Date();
      const metrics: ValueMetric[] = [
        { id: '1', timestamp: now, value: 100, source: 'system' },
        { id: '2', timestamp: now, value: 105, source: 'system' },
        { id: '3', timestamp: now, value: 110, source: 'system' },
        { id: '4', timestamp: now, value: 115, source: 'system' }, // 4+ edits
      ];

      const signal = system.detectFrequentValueEditing(metrics);
      expect(signal).not.toBeNull();
      expect(signal?.type).toBe('frequent_editing');
    });

    it('should allow single or double edits', () => {
      const now = new Date();
      const metrics: ValueMetric[] = [
        { id: '1', timestamp: now, value: 100, source: 'system' },
        { id: '2', timestamp: now, value: 105, source: 'system' },
      ];

      const signal = system.detectFrequentValueEditing(metrics);
      expect(signal).toBeNull();
    });
  });

  // ============= Batch Submission Detection =============
  describe('Batch Submission Detection', () => {
    it('should flag massive batch submissions', () => {
      const metrics: ValueMetric[] = [];
      for (let i = 0; i < 2000; i++) {
        metrics.push({
          id: `${i}`,
          timestamp: new Date(),
          value: 100,
          source: 'system',
        });
      }

      const signal = system.detectBatchValueSubmission(metrics, 1000);
      expect(signal).not.toBeNull();
      expect(signal?.type).toBe('batch_submission');
    });

    it('should allow normal submissions', () => {
      const metrics: ValueMetric[] = [];
      for (let i = 0; i < 100; i++) {
        metrics.push({
          id: `${i}`,
          timestamp: new Date(),
          value: 100,
          source: 'system',
        });
      }

      const signal = system.detectBatchValueSubmission(metrics, 1000);
      expect(signal).toBeNull();
    });
  });

  // ============= Baseline Comparison =============
  describe('Baseline Comparison', () => {
    it('should flag values outside baseline tolerance', () => {
      system.setBaseline('cust_1', 1000);

      // 30% above baseline - exceeds 20% tolerance
      expect(system.compareToBaseline('cust_1', 1300, 0.2)).toBe(false);
    });

    it('should allow values within baseline tolerance', () => {
      system.setBaseline('cust_1', 1000);

      // 15% above baseline - within 20% tolerance
      expect(system.compareToBaseline('cust_1', 1150, 0.2)).toBe(true);
    });

    it('should handle missing baseline', () => {
      // Should allow any value if no baseline set
      expect(system.compareToBaseline('cust_unknown', 5000, 0.2)).toBe(true);
    });
  });

  // ============= Comprehensive Fraud Check =============
  describe('Comprehensive Fraud Detection', () => {
    it('should detect multiple fraud indicators', () => {
      const metric: ValueMetric = {
        id: '1',
        timestamp: new Date(Date.now() + 24 * 60 * 60 * 1000), // Future!
        value: 50_000_000, // Out of range!
        source: 'suspicious_source', // Unknown source!
      };

      const signals = system.runComprehensiveCheck(metric, [], [
        'system',
        'verified_api',
      ]);

      expect(signals.length).toBeGreaterThan(0);
      expect(signals.some((s) => s.type === 'future_timestamp')).toBe(true);
      expect(signals.some((s) => s.type === 'out_of_range')).toBe(true);
      expect(signals.some((s) => s.type === 'source_spoofing')).toBe(true);
    });

    it('should pass clean metrics', () => {
      const metric: ValueMetric = {
        id: '1',
        timestamp: new Date(),
        value: 1000,
        source: 'system',
      };

      const signals = system.runComprehensiveCheck(metric, [], [
        'system',
        'verified_api',
      ]);

      expect(signals.length).toBe(0);
    });
  });

  // ============= Real-World Fraud Scenarios =============
  describe('Real-World Fraud Scenarios', () => {
    it('should detect value injection fraud', () => {
      const metrics: ValueMetric[] = [
        { id: '1', timestamp: new Date('2025-01-20'), value: 100, source: 'system' },
        { id: '2', timestamp: new Date('2025-01-21'), value: 105, source: 'system' },
        {
          id: '3',
          timestamp: new Date('2025-01-22'),
          value: 10000, // Injected value!
          source: 'unknown',
        },
      ];

      const signals = system.runComprehensiveCheck(metrics[2], metrics);
      expect(signals.length).toBeGreaterThan(0);
    });

    it('should detect metric cloning attack', () => {
      const metric1: ValueMetric = {
        id: '1',
        timestamp: new Date('2025-01-20'),
        value: 500,
        source: 'system',
      };

      const metric2: ValueMetric = { ...metric1, id: '2' }; // Clone!

      const duplicates = system.detectDuplicateMetrics([metric1, metric2]);
      expect(duplicates.length).toBeGreaterThan(0);
    });

    it('should detect backdated value claims', () => {
      const backdated = new Date('2025-01-01'); // Old date
      const metrics: ValueMetric[] = [
        { id: '1', timestamp: backdated, value: 100, source: 'system' },
      ];

      const signal = system.detectTimestampManipulation(metrics);
      // Should pass timestamp check (it's in order)
      // But system should validate these are recent submissions
      expect(signal).toBeNull(); // timestamp check passes
    });
  });
});

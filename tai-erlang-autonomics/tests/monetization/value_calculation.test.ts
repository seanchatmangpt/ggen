/**
 * Value Calculation Test Suite: 50+ Edge Cases and Anomalies
 *
 * Tests all value calculation scenarios including:
 * - Anomalies and outliers
 * - Partial value delivery
 * - Service outages and degradation
 * - Zero/negative value events
 * - Extreme values
 * - Data corruption scenarios
 */

import { describe, it, expect } from '@jest/globals';

interface ValueEvent {
  value: number;
  timestamp: Date;
  anomalyScore?: number;
}

class ValueCalculator {
  calculateValueGain(baseline: number, current: number): number {
    return Math.max(0, current - baseline);
  }

  calculateValueGainPercentage(baseline: number, current: number): number {
    if (baseline === 0) return current > 0 ? 100 : 0;
    return ((current - baseline) / Math.abs(baseline)) * 100;
  }

  detectAnomaly(value: number, mean: number, stdDev: number): boolean {
    // Z-score > 3 indicates anomaly
    if (stdDev === 0) return false;
    const zScore = Math.abs((value - mean) / stdDev);
    return zScore > 3;
  }

  calculatePartialCredit(
    expectedValue: number,
    actualValue: number,
    creditPercentage: number = 100
  ): number {
    const credit = Math.min(expectedValue, actualValue) * (creditPercentage / 100);
    return Math.max(0, credit);
  }

  detectOutage(events: ValueEvent[], threshold: number = 24): boolean {
    // Detect if no positive value events in past X hours
    const hourAgo = new Date(Date.now() - threshold * 60 * 60 * 1000);
    const recentPositive = events.filter(
      (e) => e.timestamp > hourAgo && e.value > 0
    );
    return recentPositive.length === 0;
  }

  calculateOutageImpact(
    outageHours: number,
    expectedHourlyValue: number
  ): number {
    return outageHours * expectedHourlyValue;
  }

  applySLACredit(lostValue: number, creditPercentage: number): number {
    return (lostValue * creditPercentage) / 100;
  }

  validateValueRange(value: number, min: number, max: number): boolean {
    return value >= min && value <= max;
  }

  calculateWeightedAverage(
    values: number[],
    weights: number[]
  ): number {
    if (values.length === 0) return 0;
    const sum = values.reduce((acc, val, i) => acc + val * weights[i], 0);
    const weightSum = weights.reduce((a, b) => a + b, 0);
    return weightSum > 0 ? sum / weightSum : 0;
  }

  aggregateValues(events: ValueEvent[]): number {
    if (events.length === 0) return 0;
    return events.reduce((sum, e) => sum + e.value, 0);
  }

  getMaxValue(events: ValueEvent[]): number {
    if (events.length === 0) return 0;
    return Math.max(...events.map((e) => e.value));
  }

  getMinValue(events: ValueEvent[]): number {
    if (events.length === 0) return 0;
    return Math.min(...events.map((e) => e.value));
  }

  calculateStandardDeviation(values: number[]): number {
    if (values.length === 0) return 0;
    const mean = values.reduce((a, b) => a + b, 0) / values.length;
    const variance =
      values.reduce((sum, val) => sum + Math.pow(val - mean, 2), 0) /
      values.length;
    return Math.sqrt(variance);
  }

  calculateMean(values: number[]): number {
    if (values.length === 0) return 0;
    return values.reduce((a, b) => a + b, 0) / values.length;
  }

  capValue(value: number, cap: number): number {
    return Math.min(value, cap);
  }

  applyFloorValue(value: number, floor: number): number {
    return Math.max(value, floor);
  }

  handleDataGap(lastValue: number, gapHours: number): number {
    // Linear interpolation for missing data
    const hourlyDecay = lastValue * 0.001; // Small decay per hour
    return Math.max(0, lastValue - hourlyDecay * gapHours);
  }
}

describe('Value Calculation: Edge Cases and Anomalies (50+ tests)', () => {
  let calculator: ValueCalculator;

  beforeEach(() => {
    calculator = new ValueCalculator();
  });

  // ============= Basic Value Gain Calculations =============
  describe('Basic Value Gain Calculation', () => {
    it('should calculate positive value gain', () => {
      expect(calculator.calculateValueGain(100, 150)).toBe(50);
    });

    it('should return zero when value decreases', () => {
      expect(calculator.calculateValueGain(100, 50)).toBe(0);
    });

    it('should return zero when values equal', () => {
      expect(calculator.calculateValueGain(100, 100)).toBe(0);
    });

    it('should handle large positive gains', () => {
      expect(calculator.calculateValueGain(1_000, 1_000_000)).toBe(999_000);
    });

    it('should handle negative baseline', () => {
      expect(calculator.calculateValueGain(-50, 50)).toBe(100);
    });

    it('should handle zero baseline', () => {
      expect(calculator.calculateValueGain(0, 100)).toBe(100);
    });

    it('should handle very small values', () => {
      expect(calculator.calculateValueGain(0.01, 0.02)).toBeCloseTo(0.01);
    });

    it('should handle floating point precision', () => {
      const result = calculator.calculateValueGain(0.1, 0.2);
      expect(result).toBeCloseTo(0.1);
    });
  });

  // ============= Percentage Calculations =============
  describe('Value Gain Percentage', () => {
    it('should calculate 50% gain correctly', () => {
      expect(calculator.calculateValueGainPercentage(100, 150)).toBe(50);
    });

    it('should calculate 100% gain correctly', () => {
      expect(calculator.calculateValueGainPercentage(100, 200)).toBe(100);
    });

    it('should handle zero baseline', () => {
      expect(calculator.calculateValueGainPercentage(0, 100)).toBe(100);
    });

    it('should handle negative baseline', () => {
      expect(calculator.calculateValueGainPercentage(-100, 0)).toBe(100);
    });

    it('should handle value decrease as 0% (no gain)', () => {
      expect(calculator.calculateValueGainPercentage(100, 50)).toBeGreaterThanOrEqual(0);
    });

    it('should handle 10x growth', () => {
      expect(calculator.calculateValueGainPercentage(10, 100)).toBe(900);
    });
  });

  // ============= Anomaly Detection =============
  describe('Anomaly Detection (Z-Score)', () => {
    it('should detect value > 3 standard deviations', () => {
      const mean = 100;
      const stdDev = 10;
      const anomalousValue = 140; // 4 std devs above mean
      expect(calculator.detectAnomaly(anomalousValue, mean, stdDev)).toBe(true);
    });

    it('should not flag normal values', () => {
      expect(calculator.detectAnomaly(105, 100, 10)).toBe(false);
    });

    it('should not flag values within 3 std devs', () => {
      expect(calculator.detectAnomaly(130, 100, 10)).toBe(false);
    });

    it('should handle zero standard deviation', () => {
      expect(calculator.detectAnomaly(100, 100, 0)).toBe(false);
    });

    it('should detect anomalies below mean', () => {
      const mean = 100;
      const stdDev = 10;
      const anomalousValue = 60; // 4 std devs below mean
      expect(calculator.detectAnomaly(anomalousValue, mean, stdDev)).toBe(true);
    });

    it('should handle very large values', () => {
      expect(calculator.detectAnomaly(1_000_000, 100_000, 10_000)).toBe(true);
    });
  });

  // ============= Partial Credit Scenarios =============
  describe('Partial Value Credit', () => {
    it('should award full credit for full delivery', () => {
      expect(calculator.calculatePartialCredit(100, 100, 100)).toBe(100);
    });

    it('should award 50% credit for 50% delivery', () => {
      expect(calculator.calculatePartialCredit(100, 50, 100)).toBe(50);
    });

    it('should apply SLA credit adjustment', () => {
      expect(calculator.calculatePartialCredit(100, 100, 50)).toBe(50);
    });

    it('should return 0 for zero delivery', () => {
      expect(calculator.calculatePartialCredit(100, 0, 100)).toBe(0);
    });

    it('should cap credit at expected value', () => {
      expect(calculator.calculatePartialCredit(100, 150, 100)).toBe(100);
    });

    it('should handle partial SLA credit', () => {
      expect(calculator.calculatePartialCredit(100, 80, 75)).toBe(60);
    });
  });

  // ============= Outage Detection & Impact =============
  describe('Service Outage Detection', () => {
    it('should detect 24-hour outage', () => {
      const now = new Date();
      const events: ValueEvent[] = [
        { value: 100, timestamp: new Date(now.getTime() - 48 * 60 * 60 * 1000) },
      ];
      expect(calculator.detectOutage(events, 24)).toBe(true);
    });

    it('should not flag recent positive events as outage', () => {
      const now = new Date();
      const events: ValueEvent[] = [
        { value: 100, timestamp: new Date(now.getTime() - 12 * 60 * 60 * 1000) },
      ];
      expect(calculator.detectOutage(events, 24)).toBe(false);
    });

    it('should ignore zero/negative values in outage detection', () => {
      const now = new Date();
      const events: ValueEvent[] = [
        { value: 0, timestamp: new Date(now.getTime() - 12 * 60 * 60 * 1000) },
        { value: -10, timestamp: new Date(now.getTime() - 6 * 60 * 60 * 1000) },
      ];
      expect(calculator.detectOutage(events, 24)).toBe(true);
    });

    it('should handle empty event list', () => {
      expect(calculator.detectOutage([], 24)).toBe(true);
    });

    it('should calculate outage impact correctly', () => {
      expect(calculator.calculateOutageImpact(24, 10)).toBe(240);
    });

    it('should apply SLA credit for outage', () => {
      const impact = calculator.calculateOutageImpact(24, 10);
      const credit = calculator.applySLACredit(impact, 50); // 50% credit
      expect(credit).toBe(120);
    });
  });

  // ============= Value Range Validation =============
  describe('Value Range Validation', () => {
    it('should validate value within range', () => {
      expect(calculator.validateValueRange(50, 0, 100)).toBe(true);
    });

    it('should reject value below minimum', () => {
      expect(calculator.validateValueRange(-10, 0, 100)).toBe(false);
    });

    it('should reject value above maximum', () => {
      expect(calculator.validateValueRange(110, 0, 100)).toBe(false);
    });

    it('should accept boundary values', () => {
      expect(calculator.validateValueRange(0, 0, 100)).toBe(true);
      expect(calculator.validateValueRange(100, 0, 100)).toBe(true);
    });

    it('should handle negative ranges', () => {
      expect(calculator.validateValueRange(-50, -100, 0)).toBe(true);
    });

    it('should handle floating-point ranges', () => {
      expect(calculator.validateValueRange(50.5, 50.0, 51.0)).toBe(true);
    });
  });

  // ============= Weighted Calculations =============
  describe('Weighted Average Calculation', () => {
    it('should calculate simple weighted average', () => {
      const values = [10, 20, 30];
      const weights = [1, 1, 1];
      expect(calculator.calculateWeightedAverage(values, weights)).toBe(20);
    });

    it('should handle unequal weights', () => {
      const values = [10, 20];
      const weights = [1, 2];
      expect(calculator.calculateWeightedAverage(values, weights)).toBeCloseTo(16.67, 1);
    });

    it('should return 0 for empty arrays', () => {
      expect(calculator.calculateWeightedAverage([], [])).toBe(0);
    });

    it('should handle zero weights', () => {
      const values = [10, 20];
      const weights = [0, 0];
      expect(calculator.calculateWeightedAverage(values, weights)).toBe(0);
    });

    it('should emphasize high weights', () => {
      const values = [1, 100];
      const weights = [1, 100];
      const result = calculator.calculateWeightedAverage(values, weights);
      expect(result).toBeGreaterThan(99);
    });
  });

  // ============= Aggregation Functions =============
  describe('Value Aggregation', () => {
    it('should sum all values', () => {
      const events: ValueEvent[] = [
        { value: 10, timestamp: new Date() },
        { value: 20, timestamp: new Date() },
        { value: 30, timestamp: new Date() },
      ];
      expect(calculator.aggregateValues(events)).toBe(60);
    });

    it('should handle empty event list', () => {
      expect(calculator.aggregateValues([])).toBe(0);
    });

    it('should find maximum value', () => {
      const events: ValueEvent[] = [
        { value: 10, timestamp: new Date() },
        { value: 50, timestamp: new Date() },
        { value: 30, timestamp: new Date() },
      ];
      expect(calculator.getMaxValue(events)).toBe(50);
    });

    it('should find minimum value', () => {
      const events: ValueEvent[] = [
        { value: 10, timestamp: new Date() },
        { value: 50, timestamp: new Date() },
        { value: 30, timestamp: new Date() },
      ];
      expect(calculator.getMinValue(events)).toBe(10);
    });
  });

  // ============= Statistical Calculations =============
  describe('Statistical Calculations', () => {
    it('should calculate mean correctly', () => {
      const values = [10, 20, 30];
      expect(calculator.calculateMean(values)).toBe(20);
    });

    it('should calculate standard deviation', () => {
      const values = [10, 20, 30];
      const stdDev = calculator.calculateStandardDeviation(values);
      expect(stdDev).toBeCloseTo(8.16, 1);
    });

    it('should handle single value', () => {
      expect(calculator.calculateMean([100])).toBe(100);
      expect(calculator.calculateStandardDeviation([100])).toBe(0);
    });

    it('should handle identical values', () => {
      const values = [10, 10, 10];
      expect(calculator.calculateStandardDeviation(values)).toBe(0);
    });
  });

  // ============= Value Constraints =============
  describe('Value Constraints (Caps and Floors)', () => {
    it('should cap value at maximum', () => {
      expect(calculator.capValue(150, 100)).toBe(100);
    });

    it('should not modify value below cap', () => {
      expect(calculator.capValue(75, 100)).toBe(75);
    });

    it('should apply floor value', () => {
      expect(calculator.applyFloorValue(25, 50)).toBe(50);
    });

    it('should not modify value above floor', () => {
      expect(calculator.applyFloorValue(75, 50)).toBe(75);
    });

    it('should handle zero cap', () => {
      expect(calculator.capValue(100, 0)).toBe(0);
    });

    it('should handle zero floor', () => {
      expect(calculator.applyFloorValue(-10, 0)).toBe(0);
    });
  });

  // ============= Data Gap Handling =============
  describe('Data Gap and Missing Value Handling', () => {
    it('should interpolate missing data with decay', () => {
      const result = calculator.handleDataGap(1000, 1);
      expect(result).toBeLessThan(1000);
      expect(result).toBeGreaterThan(999);
    });

    it('should not go negative with long gaps', () => {
      const result = calculator.handleDataGap(100, 1000);
      expect(result).toBeGreaterThanOrEqual(0);
    });

    it('should handle zero gap', () => {
      expect(calculator.handleDataGap(100, 0)).toBe(100);
    });

    it('should degrade gradually', () => {
      const hour1 = calculator.handleDataGap(1000, 1);
      const hour2 = calculator.handleDataGap(1000, 2);
      expect(hour1).toBeGreaterThan(hour2);
    });
  });

  // ============= Extreme Value Cases =============
  describe('Extreme Value Handling', () => {
    it('should handle very large positive values', () => {
      const result = calculator.calculateValueGain(1_000_000_000, 2_000_000_000);
      expect(result).toBe(1_000_000_000);
    });

    it('should handle very small values', () => {
      const result = calculator.calculateValueGain(0.0001, 0.0002);
      expect(result).toBeCloseTo(0.0001);
    });

    it('should handle integer overflow scenarios', () => {
      const maxSafe = Number.MAX_SAFE_INTEGER;
      const result = calculator.calculateValueGain(maxSafe - 1000, maxSafe);
      expect(result).toBe(1000);
    });

    it('should handle near-zero baselines', () => {
      const result = calculator.calculateValueGainPercentage(0.00001, 0.00002);
      expect(result).toBe(100);
    });
  });

  // ============= Composite Scenarios =============
  describe('Composite Value Scenarios (Real-World Combinations)', () => {
    it('should handle outage with partial recovery and SLA credit', () => {
      // Scenario: 12-hour outage, expected value $100/hour, 50% SLA credit
      const outageImpact = calculator.calculateOutageImpact(12, 100);
      expect(outageImpact).toBe(1200);

      const slaCreditAmount = calculator.applySLACredit(outageImpact, 50);
      expect(slaCreditAmount).toBe(600);

      const valueAfterCredit = 1200 - slaCreditAmount;
      expect(valueAfterCredit).toBe(600);
    });

    it('should handle anomaly detection with partial credit', () => {
      // Scenario: 40% delivery due to anomaly, apply 75% SLA credit
      const expectedValue = 1000;
      const actualValue = 400;
      const credit = calculator.calculatePartialCredit(expectedValue, actualValue, 75);
      expect(credit).toBe(300);
    });

    it('should handle data gaps with value cap', () => {
      // Scenario: Data gap of 48 hours, then cap at baseline
      const baseline = 100;
      const lastValue = 150;
      const interpolated = calculator.handleDataGap(lastValue, 48);
      const capped = calculator.capValue(interpolated, baseline);
      expect(capped).toBe(baseline);
    });

    it('should calculate accurate total with multiple adjustments', () => {
      // Scenario: Delivery of 80% with anomaly, apply partial credit + SLA
      const baseline = 1000;
      const actual = 800;
      const partial = calculator.calculatePartialCredit(baseline, actual, 100);
      const slaCredited = calculator.applySLACredit(partial, 90);
      expect(slaCredited).toBe(720);
    });
  });

  // ============= Fraud Detection =============
  describe('Fraud Detection Indicators', () => {
    it('should flag impossible growth rate', () => {
      // 500% growth in one period is suspicious
      const growth = calculator.calculateValueGainPercentage(100, 600);
      expect(growth).toBeGreaterThan(400);
    });

    it('should detect impossible values from anomaly score', () => {
      // Value 10x the mean should be flagged
      const mean = 100;
      const stdDev = 10;
      const suspicious = 1100;
      expect(calculator.detectAnomaly(suspicious, mean, stdDev)).toBe(true);
    });

    it('should flag negative values', () => {
      const value = -100;
      expect(calculator.validateValueRange(value, 0, 1000)).toBe(false);
    });
  });
});

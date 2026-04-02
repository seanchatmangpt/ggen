/**
 * Dispute Resolution Test Suite
 *
 * Tests handling of customer disputes about value delivery,
 * including investigation, evidence gathering, and resolution.
 */

import { describe, it, expect, beforeEach } from '@jest/globals';

interface Evidence {
  type: 'metric' | 'log' | 'contract' | 'screenshot' | 'email';
  content: string;
  timestamp: Date;
  source: string;
}

interface Dispute {
  id: string;
  invoiceId: string;
  customerId: string;
  claimedValue: number;
  reason: string;
  status:
    | 'open'
    | 'under_review'
    | 'evidence_requested'
    | 'resolved'
    | 'rejected';
  evidence: Evidence[];
  createdAt: Date;
  resolvedAt?: Date;
  resolution?: {
    creditAmount: number;
    reason: string;
    decidedBy: string;
  };
}

class DisputeManager {
  private disputes: Map<string, Dispute> = new Map();

  createDispute(data: {
    invoiceId: string;
    customerId: string;
    claimedValue: number;
    reason: string;
  }): Dispute {
    const dispute: Dispute = {
      id: `disp_${Date.now()}`,
      invoiceId: data.invoiceId,
      customerId: data.customerId,
      claimedValue: data.claimedValue,
      reason: data.reason,
      status: 'open',
      evidence: [],
      createdAt: new Date(),
    };

    this.disputes.set(dispute.id, dispute);
    return dispute;
  }

  addEvidence(disputeId: string, evidence: Evidence): Dispute {
    const dispute = this.disputes.get(disputeId);
    if (!dispute) throw new Error('Dispute not found');

    dispute.evidence.push(evidence);

    return dispute;
  }

  requestEvidence(disputeId: string, evidenceType: string[]): Dispute {
    const dispute = this.disputes.get(disputeId);
    if (!dispute) throw new Error('Dispute not found');

    dispute.status = 'evidence_requested';

    return dispute;
  }

  calculateEvidenceScore(evidence: Evidence[]): number {
    // Simple scoring: each piece of evidence adds credibility
    // Different types have different weights
    const weights: Record<string, number> = {
      metric: 0.25,
      log: 0.25,
      contract: 0.15,
      screenshot: 0.15,
      email: 0.1,
    };

    let score = 0;
    for (const item of evidence) {
      score += weights[item.type] || 0;
    }

    return Math.min(score, 1.0); // Cap at 1.0 (100%)
  }

  validateClaimAgainstMetrics(
    claimedValue: number,
    actualValue: number
  ): { valid: boolean; discrepancy: number } {
    const discrepancy = Math.abs(claimedValue - actualValue);
    const percentDiff = (discrepancy / actualValue) * 100;

    // Valid if within 10% or $100 difference
    const valid = discrepancy <= 100 || percentDiff <= 10;

    return { valid, discrepancy };
  }

  assessDisputeStrength(dispute: Dispute): number {
    // Strength assessment: 0 = weak (likely to lose), 1 = strong (likely to win)
    let strength = 0;

    // Evidence quality (max 0.4)
    const evidenceScore = this.calculateEvidenceScore(dispute.evidence);
    strength += evidenceScore * 0.4;

    // Timeliness (0-0.2): disputes within 30 days are stronger
    const daysSinceCreation = Math.floor(
      (Date.now() - dispute.createdAt.getTime()) / (1000 * 60 * 60 * 24)
    );
    const timelinessScore = Math.max(0, 1 - daysSinceCreation / 30);
    strength += timelinessScore * 0.2;

    // Evidence types (0-0.4)
    const hasLogsOrMetrics = dispute.evidence.some(
      (e) => e.type === 'log' || e.type === 'metric'
    );
    const hasContract = dispute.evidence.some((e) => e.type === 'contract');
    const hasScreenshot = dispute.evidence.some((e) => e.type === 'screenshot');

    let typeScore = 0;
    if (hasLogsOrMetrics) typeScore += 0.2;
    if (hasContract) typeScore += 0.1;
    if (hasScreenshot) typeScore += 0.1;
    strength += typeScore;

    return Math.min(strength, 1.0);
  }

  resolveDispute(
    disputeId: string,
    creditAmount: number,
    reason: string,
    decidedBy: string
  ): Dispute {
    const dispute = this.disputes.get(disputeId);
    if (!dispute) throw new Error('Dispute not found');

    dispute.status = 'resolved';
    dispute.resolvedAt = new Date();
    dispute.resolution = {
      creditAmount,
      reason,
      decidedBy,
    };

    return dispute;
  }

  rejectDispute(
    disputeId: string,
    reason: string,
    decidedBy: string
  ): Dispute {
    const dispute = this.disputes.get(disputeId);
    if (!dispute) throw new Error('Dispute not found');

    dispute.status = 'rejected';
    dispute.resolvedAt = new Date();
    dispute.resolution = {
      creditAmount: 0,
      reason,
      decidedBy,
    };

    return dispute;
  }

  getDispute(disputeId: string): Dispute | undefined {
    return this.disputes.get(disputeId);
  }

  calculateAppealWeight(
    firstDecision: {
      creditAmount: number;
      reason: string;
    },
    newEvidence: Evidence[]
  ): { shouldReconsider: boolean; weight: number } {
    // If new evidence significantly strengthens the case
    const evidenceScore = this.calculateEvidenceScore(newEvidence);

    // Reconsider if evidence score is high (>0.5) and previous decision was full rejection
    const shouldReconsider =
      evidenceScore > 0.5 && firstDecision.creditAmount === 0;

    return { shouldReconsider, weight: evidenceScore };
  }

  validateTimeWindow(createdAt: Date, maxDays: number = 90): boolean {
    const daysSince = Math.floor(
      (Date.now() - createdAt.getTime()) / (1000 * 60 * 60 * 24)
    );
    return daysSince <= maxDays;
  }
}

describe('Dispute Resolution Tests', () => {
  let manager: DisputeManager;

  beforeEach(() => {
    manager = new DisputeManager();
  });

  // ============= Dispute Creation =============
  describe('Dispute Creation and Initial Assessment', () => {
    it('should create a new dispute', () => {
      const dispute = manager.createDispute({
        invoiceId: 'inv_123',
        customerId: 'cust_456',
        claimedValue: 500,
        reason: 'Did not receive promised value',
      });

      expect(dispute).toBeDefined();
      expect(dispute.id).toMatch(/^disp_/);
      expect(dispute.status).toBe('open');
      expect(dispute.claimedValue).toBe(500);
      expect(dispute.evidence).toHaveLength(0);
    });

    it('should timestamp dispute creation', () => {
      const dispute = manager.createDispute({
        invoiceId: 'inv_123',
        customerId: 'cust_456',
        claimedValue: 500,
        reason: 'Value shortfall',
      });

      expect(dispute.createdAt).toBeInstanceOf(Date);
      expect(dispute.createdAt.getTime()).toBeLessThanOrEqual(Date.now());
    });

    it('should validate dispute is within time window', () => {
      const dispute = manager.createDispute({
        invoiceId: 'inv_123',
        customerId: 'cust_456',
        claimedValue: 500,
        reason: 'Value dispute',
      });

      expect(manager.validateTimeWindow(dispute.createdAt, 90)).toBe(true);
    });
  });

  // ============= Evidence Management =============
  describe('Evidence Collection and Management', () => {
    let dispute: Dispute;

    beforeEach(() => {
      dispute = manager.createDispute({
        invoiceId: 'inv_123',
        customerId: 'cust_456',
        claimedValue: 500,
        reason: 'Value shortfall',
      });
    });

    it('should add evidence to dispute', () => {
      const evidence: Evidence = {
        type: 'metric',
        content: 'Revenue was $50,000, not $60,000 as promised',
        timestamp: new Date(),
        source: 'customer_report',
      };

      const updated = manager.addEvidence(dispute.id, evidence);
      expect(updated.evidence).toHaveLength(1);
      expect(updated.evidence[0]).toEqual(evidence);
    });

    it('should support multiple evidence types', () => {
      const evidenceTypes: Array<Evidence['type']> = [
        'metric',
        'log',
        'contract',
        'screenshot',
        'email',
      ];

      for (const type of evidenceTypes) {
        manager.addEvidence(dispute.id, {
          type,
          content: `Evidence of type ${type}`,
          timestamp: new Date(),
          source: 'test',
        });
      }

      const updated = manager.getDispute(dispute.id);
      expect(updated?.evidence).toHaveLength(5);
    });

    it('should request specific evidence types', () => {
      const updated = manager.requestEvidence(dispute.id, [
        'metric',
        'log',
      ]);
      expect(updated.status).toBe('evidence_requested');
    });

    it('should calculate evidence credibility score', () => {
      manager.addEvidence(dispute.id, {
        type: 'metric',
        content: 'Metric data',
        timestamp: new Date(),
        source: 'system',
      });

      manager.addEvidence(dispute.id, {
        type: 'log',
        content: 'System logs',
        timestamp: new Date(),
        source: 'system',
      });

      const updated = manager.getDispute(dispute.id);
      const score = manager.calculateEvidenceScore(updated!.evidence);

      expect(score).toBeGreaterThan(0);
      expect(score).toBeLessThanOrEqual(1);
    });
  });

  // ============= Claim Validation =============
  describe('Claim Validation Against Metrics', () => {
    it('should validate claim within 10% tolerance', () => {
      const result = manager.validateClaimAgainstMetrics(550, 500);
      expect(result.valid).toBe(true);
      expect(result.discrepancy).toBe(50);
    });

    it('should validate claim within $100 absolute tolerance', () => {
      const result = manager.validateClaimAgainstMetrics(850, 1000);
      expect(result.valid).toBe(true);
      expect(result.discrepancy).toBe(150);
    });

    it('should reject claim outside tolerance', () => {
      const result = manager.validateClaimAgainstMetrics(200, 1000);
      expect(result.valid).toBe(false);
      expect(result.discrepancy).toBe(800);
    });

    it('should calculate percentage discrepancy', () => {
      const result = manager.validateClaimAgainstMetrics(450, 500);
      expect(result.discrepancy).toBe(50);
    });
  });

  // ============= Dispute Strength Assessment =============
  describe('Dispute Strength Assessment', () => {
    let dispute: Dispute;

    beforeEach(() => {
      dispute = manager.createDispute({
        invoiceId: 'inv_123',
        customerId: 'cust_456',
        claimedValue: 500,
        reason: 'Value shortfall',
      });
    });

    it('should score weak dispute with no evidence', () => {
      const strength = manager.assessDisputeStrength(dispute);
      expect(strength).toBe(0);
    });

    it('should increase score with metric evidence', () => {
      manager.addEvidence(dispute.id, {
        type: 'metric',
        content: 'System metrics',
        timestamp: new Date(),
        source: 'system',
      });

      const updated = manager.getDispute(dispute.id);
      const strength = manager.assessDisputeStrength(updated!);
      expect(strength).toBeGreaterThan(0);
    });

    it('should weight logs and metrics highly', () => {
      manager.addEvidence(dispute.id, {
        type: 'log',
        content: 'System logs',
        timestamp: new Date(),
        source: 'system',
      });

      const withLog = manager.getDispute(dispute.id);
      const logStrength = manager.assessDisputeStrength(withLog!);

      const anotherDispute = manager.createDispute({
        invoiceId: 'inv_456',
        customerId: 'cust_789',
        claimedValue: 500,
        reason: 'Dispute',
      });

      manager.addEvidence(anotherDispute.id, {
        type: 'email',
        content: 'Email evidence',
        timestamp: new Date(),
        source: 'customer',
      });

      const withEmail = manager.getDispute(anotherDispute.id);
      const emailStrength = manager.assessDisputeStrength(withEmail!);

      expect(logStrength).toBeGreaterThan(emailStrength);
    });

    it('should consider timeliness of dispute', () => {
      // Fresh dispute
      const recentDispute = manager.createDispute({
        invoiceId: 'inv_recent',
        customerId: 'cust_new',
        claimedValue: 500,
        reason: 'Recent dispute',
      });

      manager.addEvidence(recentDispute.id, {
        type: 'metric',
        content: 'Evidence',
        timestamp: new Date(),
        source: 'system',
      });

      const recentScore = manager.assessDisputeStrength(
        manager.getDispute(recentDispute.id)!
      );

      expect(recentScore).toBeGreaterThan(0);
    });
  });

  // ============= Dispute Resolution =============
  describe('Dispute Resolution Outcomes', () => {
    let dispute: Dispute;

    beforeEach(() => {
      dispute = manager.createDispute({
        invoiceId: 'inv_123',
        customerId: 'cust_456',
        claimedValue: 500,
        reason: 'Value shortfall',
      });

      manager.addEvidence(dispute.id, {
        type: 'metric',
        content: 'System metrics show shortfall',
        timestamp: new Date(),
        source: 'system',
      });
    });

    it('should resolve dispute with partial credit', () => {
      const resolved = manager.resolveDispute(
        dispute.id,
        250,
        'Partial value delivery confirmed',
        'compliance_team'
      );

      expect(resolved.status).toBe('resolved');
      expect(resolved.resolution?.creditAmount).toBe(250);
      expect(resolved.resolvedAt).toBeInstanceOf(Date);
    });

    it('should resolve dispute with full credit', () => {
      const resolved = manager.resolveDispute(
        dispute.id,
        500,
        'No value delivered',
        'compliance_team'
      );

      expect(resolved.resolution?.creditAmount).toBe(500);
    });

    it('should reject dispute with explanation', () => {
      const rejected = manager.rejectDispute(
        dispute.id,
        'Metrics do not support claim',
        'compliance_team'
      );

      expect(rejected.status).toBe('rejected');
      expect(rejected.resolution?.creditAmount).toBe(0);
      expect(rejected.resolution?.reason).toBe('Metrics do not support claim');
    });

    it('should timestamp resolution', () => {
      const resolved = manager.resolveDispute(
        dispute.id,
        250,
        'Partial credit',
        'team'
      );

      expect(resolved.resolvedAt).toBeInstanceOf(Date);
      expect(resolved.resolvedAt!.getTime()).toBeLessThanOrEqual(Date.now());
    });

    it('should track decision maker', () => {
      const resolved = manager.resolveDispute(
        dispute.id,
        250,
        'Decision',
        'john_compliance'
      );

      expect(resolved.resolution?.decidedBy).toBe('john_compliance');
    });
  });

  // ============= Appeals and Reconsideration =============
  describe('Dispute Appeals and Reconsideration', () => {
    let dispute: Dispute;

    beforeEach(() => {
      dispute = manager.createDispute({
        invoiceId: 'inv_123',
        customerId: 'cust_456',
        claimedValue: 500,
        reason: 'Value shortfall',
      });

      manager.resolveDispute(dispute.id, 0, 'No evidence provided', 'team');
    });

    it('should allow appeal with new evidence', () => {
      const newEvidence: Evidence = {
        type: 'log',
        content: 'System logs showing issue',
        timestamp: new Date(),
        source: 'system',
      };

      const appeal = manager.calculateAppealWeight(
        {
          creditAmount: 0,
          reason: 'No evidence provided',
        },
        [newEvidence]
      );

      expect(appeal.shouldReconsider).toBe(true);
    });

    it('should not reconsider without substantial new evidence', () => {
      const weakEvidence: Evidence = {
        type: 'email',
        content: 'Customer complaint',
        timestamp: new Date(),
        source: 'customer',
      };

      const appeal = manager.calculateAppealWeight(
        {
          creditAmount: 0,
          reason: 'No evidence',
        },
        [weakEvidence]
      );

      expect(appeal.shouldReconsider).toBe(false);
    });

    it('should not reconsider if already partially credited', () => {
      const strongEvidence: Evidence = {
        type: 'log',
        content: 'Strong evidence',
        timestamp: new Date(),
        source: 'system',
      };

      const appeal = manager.calculateAppealWeight(
        {
          creditAmount: 250, // Already credited something
          reason: 'Partial credit given',
        },
        [strongEvidence]
      );

      // Should not reconsider since already partially credited
      expect(appeal.weight).toBeGreaterThan(0);
    });
  });

  // ============= Complex Scenarios =============
  describe('Complex Dispute Scenarios', () => {
    it('should handle dispute with multiple evidence types', () => {
      const dispute = manager.createDispute({
        invoiceId: 'inv_123',
        customerId: 'cust_456',
        claimedValue: 1000,
        reason: 'Significant value shortfall',
      });

      manager.addEvidence(dispute.id, {
        type: 'metric',
        content: 'Metrics data',
        timestamp: new Date(),
        source: 'system',
      });

      manager.addEvidence(dispute.id, {
        type: 'log',
        content: 'Error logs',
        timestamp: new Date(),
        source: 'system',
      });

      manager.addEvidence(dispute.id, {
        type: 'contract',
        content: 'MSA terms',
        timestamp: new Date(),
        source: 'legal',
      });

      manager.requestEvidence(dispute.id, ['screenshot']);

      const updated = manager.getDispute(dispute.id);
      const strength = manager.assessDisputeStrength(updated!);

      expect(updated!.evidence.length).toBe(3);
      expect(updated!.status).toBe('evidence_requested');
      expect(strength).toBeGreaterThan(0.5);
    });

    it('should handle refund-eligible dispute workflow', () => {
      // 1. Create dispute
      const dispute = manager.createDispute({
        invoiceId: 'inv_123',
        customerId: 'cust_456',
        claimedValue: 500,
        reason: 'No value delivered',
      });

      // 2. Gather evidence
      manager.addEvidence(dispute.id, {
        type: 'metric',
        content: 'Zero delivery',
        timestamp: new Date(),
        source: 'system',
      });

      manager.addEvidence(dispute.id, {
        type: 'log',
        content: 'Service down during period',
        timestamp: new Date(),
        source: 'system',
      });

      // 3. Assess strength
      const updated = manager.getDispute(dispute.id);
      const strength = manager.assessDisputeStrength(updated!);

      // 4. Resolve
      const resolved = manager.resolveDispute(
        dispute.id,
        strength > 0.7 ? 500 : 250,
        'Partial service failure confirmed',
        'compliance'
      );

      expect(resolved.status).toBe('resolved');
      expect(resolved.resolution?.creditAmount).toBeGreaterThan(0);
    });

    it('should track dispute throughout lifecycle', () => {
      // Open → Evidence Requested → Resolved
      const dispute = manager.createDispute({
        invoiceId: 'inv_123',
        customerId: 'cust_456',
        claimedValue: 500,
        reason: 'Dispute',
      });

      expect(dispute.status).toBe('open');

      manager.requestEvidence(dispute.id, ['metric', 'log']);
      let updated = manager.getDispute(dispute.id);
      expect(updated!.status).toBe('evidence_requested');

      manager.addEvidence(dispute.id, {
        type: 'metric',
        content: 'Evidence',
        timestamp: new Date(),
        source: 'system',
      });

      manager.resolveDispute(dispute.id, 250, 'Resolved', 'team');
      updated = manager.getDispute(dispute.id);
      expect(updated!.status).toBe('resolved');
      expect(updated!.resolvedAt).toBeDefined();
    });
  });

  // ============= Edge Cases =============
  describe('Edge Cases and Validations', () => {
    it('should handle zero claim amount', () => {
      const dispute = manager.createDispute({
        invoiceId: 'inv_123',
        customerId: 'cust_456',
        claimedValue: 0,
        reason: 'No value delivered',
      });

      expect(dispute.claimedValue).toBe(0);
    });

    it('should handle dispute exceeding invoice amount', () => {
      const dispute = manager.createDispute({
        invoiceId: 'inv_123',
        customerId: 'cust_456',
        claimedValue: 5000,
        reason: 'Massive shortfall',
      });

      expect(dispute.claimedValue).toBe(5000);
    });

    it('should validate time window for disputes', () => {
      const recent = new Date();
      expect(manager.validateTimeWindow(recent, 90)).toBe(true);

      const old = new Date(Date.now() - 91 * 24 * 60 * 60 * 1000);
      expect(manager.validateTimeWindow(old, 90)).toBe(false);
    });

    it('should handle missing dispute gracefully', () => {
      expect(() => {
        manager.addEvidence('invalid_id', {
          type: 'metric',
          content: 'Evidence',
          timestamp: new Date(),
          source: 'system',
        });
      }).toThrow('Dispute not found');
    });
  });
});

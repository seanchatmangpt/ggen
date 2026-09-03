/**
 * Billing Accuracy Test Suite
 *
 * Tests invoice generation, payment processing, and refund handling
 * with focus on financial accuracy and audit trails.
 */

import { describe, it, expect, beforeEach } from '@jest/globals';

interface InvoiceItem {
  description: string;
  quantity: number;
  unitPrice: number;
  subtotal: number;
  tax: number;
  total: number;
}

interface Invoice {
  id: string;
  customerId: string;
  issueDate: Date;
  dueDate: Date;
  items: InvoiceItem[];
  subtotal: number;
  tax: number;
  total: number;
  status: 'draft' | 'issued' | 'paid' | 'refunded' | 'disputed';
  paidDate?: Date;
  auditLog: AuditEntry[];
}

interface AuditEntry {
  timestamp: Date;
  action: string;
  amount: number;
  actor: string;
  reason?: string;
}

interface Payment {
  id: string;
  invoiceId: string;
  amount: number;
  method: string;
  status: 'pending' | 'completed' | 'failed' | 'refunded';
  timestamp: Date;
  reference: string;
}

interface Refund {
  id: string;
  paymentId: string;
  invoiceId: string;
  amount: number;
  reason: string;
  status: 'pending' | 'completed' | 'failed';
  timestamp: Date;
}

class BillingEngine {
  private invoices: Map<string, Invoice> = new Map();
  private payments: Map<string, Payment> = new Map();
  private refunds: Map<string, Refund> = new Map();

  calculateItemTotal(quantity: number, unitPrice: number): number {
    const subtotal = quantity * unitPrice;
    return Math.round(subtotal * 100) / 100;
  }

  calculateTax(subtotal: number, taxRate: number): number {
    const tax = subtotal * (taxRate / 100);
    return Math.round(tax * 100) / 100;
  }

  createInvoice(data: {
    customerId: string;
    items: Array<{
      description: string;
      quantity: number;
      unitPrice: number;
    }>;
    taxRate: number;
  }): Invoice {
    const items: InvoiceItem[] = data.items.map((item) => {
      const subtotal = this.calculateItemTotal(item.quantity, item.unitPrice);
      const tax = this.calculateTax(subtotal, data.taxRate);
      return {
        description: item.description,
        quantity: item.quantity,
        unitPrice: item.unitPrice,
        subtotal,
        tax,
        total: subtotal + tax,
      };
    });

    const subtotal = Math.round(
      items.reduce((sum, i) => sum + i.subtotal, 0) * 100
    ) / 100;
    const tax = Math.round(
      items.reduce((sum, i) => sum + i.tax, 0) * 100
    ) / 100;
    const total = subtotal + tax;

    const invoice: Invoice = {
      id: `inv_${Date.now()}`,
      customerId: data.customerId,
      issueDate: new Date(),
      dueDate: new Date(Date.now() + 30 * 24 * 60 * 60 * 1000),
      items,
      subtotal,
      tax,
      total,
      status: 'draft',
      auditLog: [
        {
          timestamp: new Date(),
          action: 'created',
          amount: total,
          actor: 'system',
        },
      ],
    };

    this.invoices.set(invoice.id, invoice);
    return invoice;
  }

  issueInvoice(invoiceId: string): Invoice {
    const invoice = this.invoices.get(invoiceId);
    if (!invoice) throw new Error('Invoice not found');

    invoice.status = 'issued';
    invoice.auditLog.push({
      timestamp: new Date(),
      action: 'issued',
      amount: invoice.total,
      actor: 'system',
    });

    return invoice;
  }

  processPayment(
    invoiceId: string,
    amount: number,
    method: string
  ): { payment: Payment; invoice: Invoice } {
    const invoice = this.invoices.get(invoiceId);
    if (!invoice) throw new Error('Invoice not found');

    const payment: Payment = {
      id: `pmt_${Date.now()}`,
      invoiceId,
      amount: Math.round(amount * 100) / 100,
      method,
      status: 'completed',
      timestamp: new Date(),
      reference: `ref_${Math.random().toString(36).substr(2, 9)}`,
    };

    // Validate payment amount
    if (payment.amount > invoice.total) {
      throw new Error('Payment exceeds invoice amount');
    }

    this.payments.set(payment.id, payment);

    if (Math.abs(payment.amount - invoice.total) < 0.01) {
      // Full payment
      invoice.status = 'paid';
      invoice.paidDate = new Date();
    }

    invoice.auditLog.push({
      timestamp: new Date(),
      action: 'payment_received',
      amount: payment.amount,
      actor: 'customer',
      reason: `Payment ${payment.reference}`,
    });

    return { payment, invoice };
  }

  getInvoice(invoiceId: string): Invoice | undefined {
    return this.invoices.get(invoiceId);
  }

  issueRefund(
    paymentId: string,
    amount: number,
    reason: string
  ): Refund {
    const payment = this.payments.get(paymentId);
    if (!payment) throw new Error('Payment not found');

    const invoice = this.invoices.get(payment.invoiceId);
    if (!invoice) throw new Error('Invoice not found');

    if (amount > payment.amount) {
      throw new Error('Refund amount exceeds payment amount');
    }

    const refund: Refund = {
      id: `ref_${Date.now()}`,
      paymentId,
      invoiceId: payment.invoiceId,
      amount: Math.round(amount * 100) / 100,
      reason,
      status: 'completed',
      timestamp: new Date(),
    };

    this.refunds.set(refund.id, refund);

    if (Math.abs(refund.amount - payment.amount) < 0.01) {
      payment.status = 'refunded';
      invoice.status = 'refunded';
    }

    invoice.auditLog.push({
      timestamp: new Date(),
      action: 'refund_issued',
      amount: -refund.amount,
      actor: 'system',
      reason,
    });

    return refund;
  }

  getRefund(refundId: string): Refund | undefined {
    return this.refunds.get(refundId);
  }

  validateInvoiceBalance(invoiceId: string): boolean {
    const invoice = this.invoices.get(invoiceId);
    if (!invoice) return false;

    const calculatedTotal = invoice.items.reduce((sum, item) => sum + item.total, 0);
    return Math.abs(calculatedTotal - invoice.total) < 0.01;
  }

  getAuditTrail(invoiceId: string): AuditEntry[] {
    const invoice = this.invoices.get(invoiceId);
    return invoice?.auditLog || [];
  }

  calculateRefundWithTax(refundAmount: number, taxRate: number): number {
    const baseAmount = refundAmount / (1 + taxRate / 100);
    const tax = refundAmount - baseAmount;
    return Math.round(refundAmount * 100) / 100;
  }
}

describe('Billing Accuracy Tests', () => {
  let engine: BillingEngine;

  beforeEach(() => {
    engine = new BillingEngine();
  });

  // ============= Item Total Calculations =============
  describe('Item Total Calculations', () => {
    it('should calculate single item total correctly', () => {
      const total = engine.calculateItemTotal(1, 100);
      expect(total).toBe(100);
    });

    it('should calculate multiple quantities correctly', () => {
      const total = engine.calculateItemTotal(5, 20);
      expect(total).toBe(100);
    });

    it('should handle floating-point unit prices', () => {
      const total = engine.calculateItemTotal(3, 10.5);
      expect(total).toBeCloseTo(31.5);
    });

    it('should round to 2 decimal places', () => {
      const total = engine.calculateItemTotal(3, 10.33);
      expect(total).toBe(30.99);
    });

    it('should handle zero quantity', () => {
      expect(engine.calculateItemTotal(0, 100)).toBe(0);
    });

    it('should handle zero price', () => {
      expect(engine.calculateItemTotal(5, 0)).toBe(0);
    });
  });

  // ============= Tax Calculations =============
  describe('Tax Calculations', () => {
    it('should calculate 10% tax correctly', () => {
      const tax = engine.calculateTax(100, 10);
      expect(tax).toBe(10);
    });

    it('should calculate sales tax for $1000 at 8.875%', () => {
      const tax = engine.calculateTax(1000, 8.875);
      expect(tax).toBe(88.75);
    });

    it('should calculate VAT for international customer', () => {
      const tax = engine.calculateTax(100, 20); // UK VAT
      expect(tax).toBe(20);
    });

    it('should handle zero tax rate', () => {
      expect(engine.calculateTax(100, 0)).toBe(0);
    });

    it('should round tax to 2 decimal places', () => {
      const tax = engine.calculateTax(123.45, 7);
      expect(tax).toBe(8.64);
    });

    it('should handle very small amounts with tax', () => {
      const tax = engine.calculateTax(0.01, 10);
      expect(tax).toBeCloseTo(0.001, 3);
    });
  });

  // ============= Invoice Creation =============
  describe('Invoice Creation and Structure', () => {
    it('should create invoice with single item', () => {
      const invoice = engine.createInvoice({
        customerId: 'cust_123',
        items: [
          {
            description: 'Service delivery',
            quantity: 1,
            unitPrice: 1000,
          },
        ],
        taxRate: 10,
      });

      expect(invoice.id).toBeDefined();
      expect(invoice.customerId).toBe('cust_123');
      expect(invoice.items.length).toBe(1);
      expect(invoice.status).toBe('draft');
      expect(invoice.subtotal).toBe(1000);
      expect(invoice.tax).toBe(100);
      expect(invoice.total).toBe(1100);
    });

    it('should create invoice with multiple items', () => {
      const invoice = engine.createInvoice({
        customerId: 'cust_123',
        items: [
          { description: 'Item 1', quantity: 2, unitPrice: 50 },
          { description: 'Item 2', quantity: 3, unitPrice: 75 },
          { description: 'Item 3', quantity: 1, unitPrice: 200 },
        ],
        taxRate: 10,
      });

      expect(invoice.items.length).toBe(3);
      const expectedSubtotal = 100 + 225 + 200; // 525
      expect(invoice.subtotal).toBe(expectedSubtotal);
      expect(invoice.tax).toBe(52.5);
      expect(invoice.total).toBe(577.5);
    });

    it('should validate invoice balance', () => {
      const invoice = engine.createInvoice({
        customerId: 'cust_123',
        items: [
          { description: 'Service', quantity: 1, unitPrice: 1000 },
        ],
        taxRate: 10,
      });

      expect(engine.validateInvoiceBalance(invoice.id)).toBe(true);
    });

    it('should initialize audit log on creation', () => {
      const invoice = engine.createInvoice({
        customerId: 'cust_123',
        items: [
          { description: 'Service', quantity: 1, unitPrice: 100 },
        ],
        taxRate: 10,
      });

      expect(invoice.auditLog.length).toBeGreaterThan(0);
      expect(invoice.auditLog[0].action).toBe('created');
    });
  });

  // ============= Invoice Issuance =============
  describe('Invoice Issuance', () => {
    it('should change status from draft to issued', () => {
      const invoice = engine.createInvoice({
        customerId: 'cust_123',
        items: [
          { description: 'Service', quantity: 1, unitPrice: 100 },
        ],
        taxRate: 10,
      });

      const issued = engine.issueInvoice(invoice.id);
      expect(issued.status).toBe('issued');
    });

    it('should add audit entry on issuance', () => {
      const invoice = engine.createInvoice({
        customerId: 'cust_123',
        items: [
          { description: 'Service', quantity: 1, unitPrice: 100 },
        ],
        taxRate: 10,
      });

      const initialEntries = invoice.auditLog.length;
      engine.issueInvoice(invoice.id);
      const updated = engine.getInvoice(invoice.id);

      expect(updated!.auditLog.length).toBe(initialEntries + 1);
      expect(updated!.auditLog[updated!.auditLog.length - 1].action).toBe('issued');
    });
  });

  // ============= Payment Processing =============
  describe('Payment Processing', () => {
    it('should process full payment for invoice', () => {
      const invoice = engine.createInvoice({
        customerId: 'cust_123',
        items: [
          { description: 'Service', quantity: 1, unitPrice: 1000 },
        ],
        taxRate: 10,
      });

      const { payment, invoice: updatedInvoice } = engine.processPayment(
        invoice.id,
        invoice.total,
        'credit_card'
      );

      expect(payment.status).toBe('completed');
      expect(payment.amount).toBe(1100);
      expect(updatedInvoice.status).toBe('paid');
      expect(updatedInvoice.paidDate).toBeInstanceOf(Date);
    });

    it('should track payment reference', () => {
      const invoice = engine.createInvoice({
        customerId: 'cust_123',
        items: [
          { description: 'Service', quantity: 1, unitPrice: 100 },
        ],
        taxRate: 10,
      });

      const { payment } = engine.processPayment(invoice.id, invoice.total, 'card');

      expect(payment.reference).toBeDefined();
      expect(payment.reference).toMatch(/^ref_/);
    });

    it('should reject overpayment', () => {
      const invoice = engine.createInvoice({
        customerId: 'cust_123',
        items: [
          { description: 'Service', quantity: 1, unitPrice: 100 },
        ],
        taxRate: 10,
      });

      expect(() => {
        engine.processPayment(invoice.id, invoice.total + 100, 'card');
      }).toThrow('Payment exceeds invoice amount');
    });

    it('should add audit entry for payment', () => {
      const invoice = engine.createInvoice({
        customerId: 'cust_123',
        items: [
          { description: 'Service', quantity: 1, unitPrice: 100 },
        ],
        taxRate: 10,
      });

      const initialEntries = invoice.auditLog.length;
      engine.processPayment(invoice.id, invoice.total, 'card');
      const updated = engine.getInvoice(invoice.id);

      expect(updated!.auditLog.length).toBeGreaterThan(initialEntries);
      const lastEntry = updated!.auditLog[updated!.auditLog.length - 1];
      expect(lastEntry.action).toBe('payment_received');
    });

    it('should handle multiple partial payments', () => {
      const invoice = engine.createInvoice({
        customerId: 'cust_123',
        items: [
          { description: 'Service', quantity: 1, unitPrice: 1000 },
        ],
        taxRate: 10,
      });

      const { invoice: inv1 } = engine.processPayment(invoice.id, 550, 'card');
      expect(inv1.status).not.toBe('paid'); // Partial payment

      const { invoice: inv2 } = engine.processPayment(invoice.id, 550, 'card');
      expect(inv2.status).toBe('paid'); // Full payment now
    });
  });

  // ============= Refund Processing =============
  describe('Refund Processing', () => {
    it('should process full refund', () => {
      const invoice = engine.createInvoice({
        customerId: 'cust_123',
        items: [
          { description: 'Service', quantity: 1, unitPrice: 1000 },
        ],
        taxRate: 10,
      });

      const { payment } = engine.processPayment(invoice.id, invoice.total, 'card');

      const refund = engine.issueRefund(payment.id, payment.amount, 'Customer request');

      expect(refund.status).toBe('completed');
      expect(refund.amount).toBe(1100);
      expect(refund.reason).toBe('Customer request');
    });

    it('should process partial refund', () => {
      const invoice = engine.createInvoice({
        customerId: 'cust_123',
        items: [
          { description: 'Service', quantity: 1, unitPrice: 1000 },
        ],
        taxRate: 10,
      });

      const { payment } = engine.processPayment(invoice.id, invoice.total, 'card');

      const refund = engine.issueRefund(payment.id, 550, 'Partial refund');

      expect(refund.amount).toBe(550);
      const retrieved = engine.getRefund(refund.id);
      expect(retrieved).toBeDefined();
    });

    it('should reject refund exceeding payment', () => {
      const invoice = engine.createInvoice({
        customerId: 'cust_123',
        items: [
          { description: 'Service', quantity: 1, unitPrice: 100 },
        ],
        taxRate: 10,
      });

      const { payment } = engine.processPayment(invoice.id, invoice.total, 'card');

      expect(() => {
        engine.issueRefund(payment.id, payment.amount + 100, 'Invalid refund');
      }).toThrow('Refund amount exceeds payment amount');
    });

    it('should add audit entry for refund', () => {
      const invoice = engine.createInvoice({
        customerId: 'cust_123',
        items: [
          { description: 'Service', quantity: 1, unitPrice: 100 },
        ],
        taxRate: 10,
      });

      const { payment } = engine.processPayment(invoice.id, invoice.total, 'card');
      const initialEntries = engine.getAuditTrail(invoice.id).length;

      engine.issueRefund(payment.id, 100, 'Test refund');
      const auditLog = engine.getAuditTrail(invoice.id);

      expect(auditLog.length).toBeGreaterThan(initialEntries);
      const lastEntry = auditLog[auditLog.length - 1];
      expect(lastEntry.action).toBe('refund_issued');
      expect(lastEntry.amount).toBe(-100);
    });
  });

  // ============= Audit Trail =============
  describe('Audit Trail and Traceability', () => {
    it('should maintain complete audit log', () => {
      const invoice = engine.createInvoice({
        customerId: 'cust_123',
        items: [
          { description: 'Service', quantity: 1, unitPrice: 1000 },
        ],
        taxRate: 10,
      });

      engine.issueInvoice(invoice.id);
      const { payment } = engine.processPayment(invoice.id, invoice.total, 'card');
      engine.issueRefund(payment.id, 500, 'Partial refund');

      const auditLog = engine.getAuditTrail(invoice.id);

      expect(auditLog.length).toBeGreaterThanOrEqual(4);
      expect(auditLog[0].action).toBe('created');
      expect(auditLog.some((e) => e.action === 'issued')).toBe(true);
      expect(auditLog.some((e) => e.action === 'payment_received')).toBe(true);
      expect(auditLog.some((e) => e.action === 'refund_issued')).toBe(true);
    });

    it('should track all financial changes', () => {
      const invoice = engine.createInvoice({
        customerId: 'cust_123',
        items: [
          { description: 'Service', quantity: 1, unitPrice: 1000 },
        ],
        taxRate: 10,
      });

      const { payment } = engine.processPayment(invoice.id, 1100, 'card');
      engine.issueRefund(payment.id, 220, 'Partial refund');

      const auditLog = engine.getAuditTrail(invoice.id);
      const amounts = auditLog.map((e) => e.amount);

      expect(amounts).toContain(1100); // Initial invoice
      expect(amounts).toContain(1100); // Payment
      expect(amounts).toContain(-220); // Refund
    });

    it('should track actor information', () => {
      const invoice = engine.createInvoice({
        customerId: 'cust_123',
        items: [
          { description: 'Service', quantity: 1, unitPrice: 100 },
        ],
        taxRate: 10,
      });

      engine.issueInvoice(invoice.id);
      engine.processPayment(invoice.id, invoice.total, 'card');

      const auditLog = engine.getAuditTrail(invoice.id);

      expect(auditLog.some((e) => e.actor === 'system')).toBe(true);
      expect(auditLog.some((e) => e.actor === 'customer')).toBe(true);
    });
  });

  // ============= Complex Scenarios =============
  describe('Complex Billing Scenarios', () => {
    it('should handle invoice with multiple items and tax', () => {
      const invoice = engine.createInvoice({
        customerId: 'cust_123',
        items: [
          { description: 'Service A', quantity: 2, unitPrice: 100 },
          { description: 'Service B', quantity: 1, unitPrice: 200 },
          { description: 'Service C', quantity: 4, unitPrice: 50 },
        ],
        taxRate: 8.875,
      });

      // Subtotal: 200 + 200 + 200 = 600
      // Tax: 600 * 0.08875 = 53.25
      // Total: 653.25

      expect(invoice.subtotal).toBe(600);
      expect(invoice.tax).toBeCloseTo(53.25, 1);
      expect(invoice.total).toBeCloseTo(653.25, 1);
    });

    it('should handle full invoice lifecycle', () => {
      // 1. Create
      const invoice = engine.createInvoice({
        customerId: 'cust_123',
        items: [
          { description: 'Service', quantity: 1, unitPrice: 1000 },
        ],
        taxRate: 10,
      });

      expect(invoice.status).toBe('draft');

      // 2. Issue
      const issued = engine.issueInvoice(invoice.id);
      expect(issued.status).toBe('issued');

      // 3. Payment
      const { payment } = engine.processPayment(invoice.id, 1100, 'card');
      expect(payment.status).toBe('completed');

      const paid = engine.getInvoice(invoice.id);
      expect(paid?.status).toBe('paid');

      // 4. Audit trail complete
      const auditLog = engine.getAuditTrail(invoice.id);
      expect(auditLog.length).toBeGreaterThanOrEqual(3);
    });

    it('should handle refund with tax adjustment', () => {
      const invoice = engine.createInvoice({
        customerId: 'cust_123',
        items: [
          { description: 'Service', quantity: 1, unitPrice: 100 },
        ],
        taxRate: 10,
      });

      const { payment } = engine.processPayment(invoice.id, invoice.total, 'card');
      const refundAmount = engine.calculateRefundWithTax(100, 10);

      expect(refundAmount).toBe(100);

      const refund = engine.issueRefund(payment.id, refundAmount, 'Tax adjustment');
      expect(refund.amount).toBe(100);
    });
  });

  // ============= Edge Cases =============
  describe('Edge Cases and Error Handling', () => {
    it('should handle penny rounding correctly', () => {
      const invoice = engine.createInvoice({
        customerId: 'cust_123',
        items: [
          { description: 'Item 1', quantity: 3, unitPrice: 0.33 },
          { description: 'Item 2', quantity: 3, unitPrice: 0.34 },
        ],
        taxRate: 7,
      });

      expect(invoice.subtotal).toBe(2.01);
      expect(invoice.tax).toBeCloseTo(0.14, 2);
    });

    it('should handle very large invoices', () => {
      const invoice = engine.createInvoice({
        customerId: 'cust_123',
        items: [
          { description: 'Large service', quantity: 10000, unitPrice: 10000 },
        ],
        taxRate: 10,
      });

      expect(invoice.subtotal).toBe(100_000_000);
      expect(invoice.total).toBe(110_000_000);
    });

    it('should handle zero-amount items', () => {
      const invoice = engine.createInvoice({
        customerId: 'cust_123',
        items: [
          { description: 'Free service', quantity: 1, unitPrice: 0 },
          { description: 'Paid service', quantity: 1, unitPrice: 100 },
        ],
        taxRate: 10,
      });

      expect(invoice.subtotal).toBe(100);
      expect(invoice.tax).toBe(10);
    });
  });
});

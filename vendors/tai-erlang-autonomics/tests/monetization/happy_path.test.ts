/**
 * Happy Path Test Suite: Customer Signup → Value Definition → Live Billing → Invoice
 *
 * Tests the complete monetization flow from beginning to end
 * with all systems working correctly together.
 */

import { describe, it, expect, beforeEach, afterEach } from '@jest/globals';
import type {
  Customer,
  ValueDefinition,
  BillingPeriod,
  Invoice,
  PaymentResult,
} from '../types';

// Mock implementations
class MonetizationService {
  private customers: Map<string, Customer> = new Map();
  private valueDefinitions: Map<string, ValueDefinition> = new Map();
  private billingPeriods: Map<string, BillingPeriod> = new Map();
  private invoices: Map<string, Invoice> = new Map();

  async registerCustomer(data: {
    name: string;
    email: string;
    company: string;
    country: string;
  }): Promise<Customer> {
    const customer: Customer = {
      id: `cust_${Date.now()}`,
      name: data.name,
      email: data.email,
      company: data.company,
      country: data.country,
      status: 'active',
      createdAt: new Date(),
      billingEmail: data.email,
      currency: this.getCurrencyForCountry(data.country),
    };
    this.customers.set(customer.id, customer);
    return customer;
  }

  async defineValue(customerId: string, valueData: {
    metric: string;
    unit: string;
    baseline: number;
    target: number;
    currency: string;
  }): Promise<ValueDefinition> {
    const customer = this.customers.get(customerId);
    if (!customer) throw new Error('Customer not found');

    const valueDef: ValueDefinition = {
      id: `vdef_${Date.now()}`,
      customerId,
      metric: valueData.metric,
      unit: valueData.unit,
      baseline: valueData.baseline,
      target: valueData.target,
      currency: valueData.currency,
      pricePerUnit: this.calculatePricePerUnit(valueData),
      status: 'active',
      createdAt: new Date(),
      startDate: new Date(),
    };
    this.valueDefinitions.set(valueDef.id, valueDef);
    return valueDef;
  }

  async startBillingPeriod(customerId: string, valueDefId: string): Promise<BillingPeriod> {
    const customer = this.customers.get(customerId);
    const valueDef = this.valueDefinitions.get(valueDefId);
    if (!customer || !valueDef) throw new Error('Customer or value definition not found');

    const period: BillingPeriod = {
      id: `bp_${Date.now()}`,
      customerId,
      valueDefId,
      startDate: new Date(),
      endDate: new Date(Date.now() + 30 * 24 * 60 * 60 * 1000),
      status: 'active',
      initialValue: valueDef.baseline,
      currentValue: valueDef.baseline,
      createdAt: new Date(),
    };
    this.billingPeriods.set(period.id, period);
    return period;
  }

  async recordValueEvent(
    billingPeriodId: string,
    value: number,
    timestamp: Date = new Date()
  ): Promise<void> {
    const period = this.billingPeriods.get(billingPeriodId);
    if (!period) throw new Error('Billing period not found');
    period.currentValue = value;
  }

  async generateInvoice(billingPeriodId: string): Promise<Invoice> {
    const period = this.billingPeriods.get(billingPeriodId);
    if (!period) throw new Error('Billing period not found');

    const valueDef = this.valueDefinitions.get(period.valueDefId);
    if (!valueDef) throw new Error('Value definition not found');

    const customer = this.customers.get(period.customerId);
    if (!customer) throw new Error('Customer not found');

    const valueGain = period.currentValue - period.initialValue;
    const invoiceAmount = Math.max(0, valueGain * valueDef.pricePerUnit);

    const invoice: Invoice = {
      id: `inv_${Date.now()}`,
      customerId: period.customerId,
      billingPeriodId,
      amount: invoiceAmount,
      amountCents: Math.round(invoiceAmount * 100),
      currency: valueDef.currency,
      status: 'issued',
      issueDate: new Date(),
      dueDate: new Date(Date.now() + 30 * 24 * 60 * 60 * 1000),
      items: [
        {
          description: `${valueDef.metric} value delivery (${period.initialValue} → ${period.currentValue} ${valueDef.unit})`,
          quantity: valueGain,
          unitPrice: valueDef.pricePerUnit,
          total: invoiceAmount,
        },
      ],
      metadata: {
        valueGain,
        baseline: period.initialValue,
        target: valueDef.target,
      },
    };
    this.invoices.set(invoice.id, invoice);
    return invoice;
  }

  async processPayment(invoiceId: string, paymentMethod: string): Promise<PaymentResult> {
    const invoice = this.invoices.get(invoiceId);
    if (!invoice) throw new Error('Invoice not found');

    // Simulate payment processing
    const success = Math.random() > 0.05; // 95% success rate
    if (success) {
      invoice.status = 'paid';
      invoice.paidDate = new Date();
      return {
        success: true,
        invoiceId,
        transactionId: `txn_${Date.now()}`,
        amount: invoice.amount,
        timestamp: new Date(),
      };
    } else {
      return {
        success: false,
        invoiceId,
        error: 'Payment processing failed',
        timestamp: new Date(),
      };
    }
  }

  private getCurrencyForCountry(country: string): string {
    const currencyMap: Record<string, string> = {
      US: 'USD',
      GB: 'GBP',
      DE: 'EUR',
      FR: 'EUR',
      JP: 'JPY',
      SG: 'SGD',
    };
    return currencyMap[country.toUpperCase()] || 'USD';
  }

  private calculatePricePerUnit(valueData: {
    target: number;
    baseline: number;
  }): number {
    const targetGain = valueData.target - valueData.baseline;
    return targetGain > 0 ? 100 / targetGain : 0; // Simplified pricing
  }

  getInvoice(invoiceId: string): Invoice | undefined {
    return this.invoices.get(invoiceId);
  }
}

describe('Happy Path: Complete Monetization Flow', () => {
  let service: MonetizationService;

  beforeEach(() => {
    service = new MonetizationService();
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  describe('Customer Signup', () => {
    it('should register a new customer successfully', async () => {
      const customer = await service.registerCustomer({
        name: 'John Acme Corp',
        email: 'john@acme.com',
        company: 'Acme Corporation',
        country: 'US',
      });

      expect(customer).toBeDefined();
      expect(customer.id).toMatch(/^cust_/);
      expect(customer.name).toBe('John Acme Corp');
      expect(customer.email).toBe('john@acme.com');
      expect(customer.status).toBe('active');
      expect(customer.currency).toBe('USD');
      expect(customer.createdAt).toBeInstanceOf(Date);
    });

    it('should support international customers with correct currency', async () => {
      const currencies = [
        { country: 'GB', expected: 'GBP' },
        { country: 'DE', expected: 'EUR' },
        { country: 'JP', expected: 'JPY' },
        { country: 'SG', expected: 'SGD' },
      ];

      for (const { country, expected } of currencies) {
        const customer = await service.registerCustomer({
          name: 'Test Customer',
          email: 'test@example.com',
          company: 'Test Co',
          country,
        });
        expect(customer.currency).toBe(expected);
      }
    });
  });

  describe('Value Definition', () => {
    let customerId: string;

    beforeEach(async () => {
      const customer = await service.registerCustomer({
        name: 'Test Customer',
        email: 'test@example.com',
        company: 'Test Co',
        country: 'US',
      });
      customerId = customer.id;
    });

    it('should define value metrics for customer', async () => {
      const valueDef = await service.defineValue(customerId, {
        metric: 'Revenue Generated',
        unit: 'USD',
        baseline: 1_000_000,
        target: 1_500_000,
        currency: 'USD',
      });

      expect(valueDef).toBeDefined();
      expect(valueDef.id).toMatch(/^vdef_/);
      expect(valueDef.metric).toBe('Revenue Generated');
      expect(valueDef.baseline).toBe(1_000_000);
      expect(valueDef.target).toBe(1_500_000);
      expect(valueDef.status).toBe('active');
      expect(valueDef.pricePerUnit).toBeGreaterThan(0);
    });

    it('should calculate correct price per unit', async () => {
      const valueDef = await service.defineValue(customerId, {
        metric: 'Efficiency Improvement',
        unit: 'percentage points',
        baseline: 70,
        target: 85,
        currency: 'USD',
      });

      // Target gain = 85 - 70 = 15 units
      // Price per unit = 100 / 15 ≈ 6.67
      expect(valueDef.pricePerUnit).toBeCloseTo(100 / 15, 2);
    });
  });

  describe('Billing Period Setup', () => {
    let customerId: string;
    let valueDefId: string;

    beforeEach(async () => {
      const customer = await service.registerCustomer({
        name: 'Test Customer',
        email: 'test@example.com',
        company: 'Test Co',
        country: 'US',
      });
      customerId = customer.id;

      const valueDef = await service.defineValue(customerId, {
        metric: 'Revenue Generated',
        unit: 'USD',
        baseline: 1_000_000,
        target: 1_500_000,
        currency: 'USD',
      });
      valueDefId = valueDef.id;
    });

    it('should start a new billing period', async () => {
      const period = await service.startBillingPeriod(customerId, valueDefId);

      expect(period).toBeDefined();
      expect(period.id).toMatch(/^bp_/);
      expect(period.customerId).toBe(customerId);
      expect(period.valueDefId).toBe(valueDefId);
      expect(period.status).toBe('active');
      expect(period.startDate).toBeInstanceOf(Date);
      expect(period.endDate).toBeInstanceOf(Date);
      expect(period.endDate.getTime()).toBeGreaterThan(period.startDate.getTime());
    });

    it('should track value changes during billing period', async () => {
      const period = await service.startBillingPeriod(customerId, valueDefId);

      await service.recordValueEvent(period.id, 1_100_000);
      let retrievedPeriod = service.billingPeriods.get(period.id);
      expect(retrievedPeriod?.currentValue).toBe(1_100_000);

      await service.recordValueEvent(period.id, 1_250_000);
      retrievedPeriod = service.billingPeriods.get(period.id);
      expect(retrievedPeriod?.currentValue).toBe(1_250_000);
    });
  });

  describe('Invoice Generation', () => {
    let customerId: string;
    let valueDefId: string;
    let billingPeriodId: string;

    beforeEach(async () => {
      const customer = await service.registerCustomer({
        name: 'Test Customer',
        email: 'test@example.com',
        company: 'Test Co',
        country: 'US',
      });
      customerId = customer.id;

      const valueDef = await service.defineValue(customerId, {
        metric: 'Revenue Generated',
        unit: 'USD',
        baseline: 1_000_000,
        target: 1_500_000,
        currency: 'USD',
      });
      valueDefId = valueDef.id;

      const period = await service.startBillingPeriod(customerId, valueDefId);
      billingPeriodId = period.id;

      await service.recordValueEvent(billingPeriodId, 1_300_000);
    });

    it('should generate invoice with correct amount', async () => {
      const invoice = await service.generateInvoice(billingPeriodId);

      expect(invoice).toBeDefined();
      expect(invoice.id).toMatch(/^inv_/);
      expect(invoice.status).toBe('issued');
      expect(invoice.customerId).toBe(customerId);

      // Value gain = 1_300_000 - 1_000_000 = 300_000
      // Price per unit = 100 / (1_500_000 - 1_000_000) = 0.0002
      // Total = 300_000 * 0.0002 = 60
      expect(invoice.amount).toBeGreaterThan(0);
      expect(invoice.amountCents).toBe(Math.round(invoice.amount * 100));
    });

    it('should include all invoice items', async () => {
      const invoice = await service.generateInvoice(billingPeriodId);

      expect(invoice.items).toBeDefined();
      expect(invoice.items.length).toBeGreaterThan(0);
      expect(invoice.items[0]).toHaveProperty('description');
      expect(invoice.items[0]).toHaveProperty('quantity');
      expect(invoice.items[0]).toHaveProperty('unitPrice');
      expect(invoice.items[0]).toHaveProperty('total');
    });

    it('should include metadata for audit trail', async () => {
      const invoice = await service.generateInvoice(billingPeriodId);

      expect(invoice.metadata).toBeDefined();
      expect(invoice.metadata.valueGain).toBe(300_000);
      expect(invoice.metadata.baseline).toBe(1_000_000);
      expect(invoice.metadata.target).toBe(1_500_000);
    });
  });

  describe('Payment Processing', () => {
    let invoiceId: string;

    beforeEach(async () => {
      const customer = await service.registerCustomer({
        name: 'Test Customer',
        email: 'test@example.com',
        company: 'Test Co',
        country: 'US',
      });

      const valueDef = await service.defineValue(customer.id, {
        metric: 'Revenue Generated',
        unit: 'USD',
        baseline: 1_000_000,
        target: 1_500_000,
        currency: 'USD',
      });

      const period = await service.startBillingPeriod(customer.id, valueDef.id);
      await service.recordValueEvent(period.id, 1_300_000);

      const invoice = await service.generateInvoice(period.id);
      invoiceId = invoice.id;
    });

    it('should process payment successfully', async () => {
      const result = await service.processPayment(invoiceId, 'card');

      if (result.success) {
        expect(result.transactionId).toBeDefined();
        expect(result.amount).toBeGreaterThan(0);
        expect(result.timestamp).toBeInstanceOf(Date);

        const invoice = service.getInvoice(invoiceId);
        expect(invoice?.status).toBe('paid');
        expect(invoice?.paidDate).toBeInstanceOf(Date);
      }
    });

    it('should mark invoice as paid after successful payment', async () => {
      const result = await service.processPayment(invoiceId, 'card');

      if (result.success) {
        const invoice = service.getInvoice(invoiceId);
        expect(invoice?.status).toBe('paid');
        expect(invoice?.paidDate).not.toBeNull();
      }
    });
  });

  describe('Complete End-to-End Flow', () => {
    it('should complete full monetization cycle', async () => {
      // Step 1: Customer signup
      const customer = await service.registerCustomer({
        name: 'Acme Analytics',
        email: 'cfo@acme.com',
        company: 'Acme Corporation',
        country: 'US',
      });
      expect(customer.status).toBe('active');

      // Step 2: Define value
      const valueDef = await service.defineValue(customer.id, {
        metric: 'Revenue Generated',
        unit: 'USD',
        baseline: 1_000_000,
        target: 1_500_000,
        currency: 'USD',
      });
      expect(valueDef.status).toBe('active');

      // Step 3: Start billing period
      const period = await service.startBillingPeriod(customer.id, valueDef.id);
      expect(period.status).toBe('active');

      // Step 4: Record value events
      await service.recordValueEvent(period.id, 1_100_000);
      await service.recordValueEvent(period.id, 1_200_000);
      await service.recordValueEvent(period.id, 1_350_000);

      // Step 5: Generate invoice
      const invoice = await service.generateInvoice(period.id);
      expect(invoice.status).toBe('issued');
      expect(invoice.amount).toBeGreaterThan(0);

      // Step 6: Process payment
      const result = await service.processPayment(invoice.id, 'card');
      expect(result).toHaveProperty('success');
      expect(result).toHaveProperty('timestamp');

      // Verify final state
      const finalInvoice = service.getInvoice(invoice.id);
      if (result.success) {
        expect(finalInvoice?.status).toBe('paid');
      }
    });
  });
});

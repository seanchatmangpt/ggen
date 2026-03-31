/**
 * Multi-Currency and Tax Compliance Test Suite
 *
 * Tests billing in multiple currencies with correct exchange rates,
 * VAT/GST/Sales tax calculation and reporting.
 */

import { describe, it, expect, beforeEach } from '@jest/globals';

interface ExchangeRate {
  from: string;
  to: string;
  rate: number;
  timestamp: Date;
}

interface TaxRegion {
  code: string;
  name: string;
  taxRate: number;
  type: 'sales_tax' | 'vat' | 'gst';
}

interface Invoice {
  amount: number;
  currency: string;
  taxRegion: TaxRegion;
  subtotal: number;
  tax: number;
  total: number;
}

class MultiCurrencyEngine {
  private exchangeRates: Map<string, ExchangeRate> = new Map();
  private taxRegions: Map<string, TaxRegion> = new Map();

  // Initialize standard exchange rates (as of test date)
  constructor() {
    // Base rates relative to USD
    const baseRates = [
      { from: 'USD', to: 'GBP', rate: 0.79 },
      { from: 'USD', to: 'EUR', rate: 0.92 },
      { from: 'USD', to: 'JPY', rate: 149.5 },
      { from: 'USD', to: 'SGD', rate: 1.34 },
      { from: 'GBP', to: 'USD', rate: 1.265 },
      { from: 'EUR', to: 'USD', rate: 1.087 },
      { from: 'JPY', to: 'USD', rate: 0.00668 },
      { from: 'SGD', to: 'USD', rate: 0.746 },
    ];

    for (const rate of baseRates) {
      const key = `${rate.from}_${rate.to}`;
      this.exchangeRates.set(key, {
        from: rate.from,
        to: rate.to,
        rate: rate.rate,
        timestamp: new Date(),
      });
    }

    // Initialize tax regions
    const regions = [
      {
        code: 'US',
        name: 'United States',
        taxRate: 8.875, // Average sales tax
        type: 'sales_tax' as const,
      },
      {
        code: 'GB',
        name: 'United Kingdom',
        taxRate: 20,
        type: 'vat' as const,
      },
      {
        code: 'DE',
        name: 'Germany',
        taxRate: 19,
        type: 'vat' as const,
      },
      {
        code: 'FR',
        name: 'France',
        taxRate: 20,
        type: 'vat' as const,
      },
      {
        code: 'JP',
        name: 'Japan',
        taxRate: 10,
        type: 'consumption_tax' as const,
      },
      {
        code: 'SG',
        name: 'Singapore',
        taxRate: 8,
        type: 'gst' as const,
      },
      {
        code: 'AU',
        name: 'Australia',
        taxRate: 10,
        type: 'gst' as const,
      },
    ];

    for (const region of regions) {
      this.taxRegions.set(region.code, region as any);
    }
  }

  convertCurrency(
    amount: number,
    fromCurrency: string,
    toCurrency: string
  ): number {
    if (fromCurrency === toCurrency) return amount;

    const key = `${fromCurrency}_${toCurrency}`;
    const rate = this.exchangeRates.get(key);

    if (!rate) {
      throw new Error(`Exchange rate not found: ${key}`);
    }

    return Math.round((amount * rate.rate) * 100) / 100;
  }

  convertToUSD(amount: number, currency: string): number {
    if (currency === 'USD') return amount;
    return this.convertCurrency(amount, currency, 'USD');
  }

  convertFromUSD(amount: number, currency: string): number {
    if (currency === 'USD') return amount;
    return this.convertCurrency(amount, 'USD', currency);
  }

  calculateTax(
    amount: number,
    taxRegionCode: string
  ): { subtotal: number; tax: number; total: number } {
    const region = this.taxRegions.get(taxRegionCode);
    if (!region) {
      throw new Error(`Tax region not found: ${taxRegionCode}`);
    }

    const subtotal = Math.round(amount * 100) / 100;
    const tax = Math.round((subtotal * region.taxRate) / 100 * 100) / 100;
    const total = Math.round((subtotal + tax) * 100) / 100;

    return { subtotal, tax, total };
  }

  createInvoice(
    amount: number,
    currency: string,
    taxRegionCode: string
  ): Invoice {
    const region = this.taxRegions.get(taxRegionCode);
    if (!region) {
      throw new Error(`Tax region not found: ${taxRegionCode}`);
    }

    const { subtotal, tax, total } = this.calculateTax(amount, taxRegionCode);

    return {
      amount,
      currency,
      taxRegion: region,
      subtotal,
      tax,
      total,
    };
  }

  convertInvoice(
    invoice: Invoice,
    toCurrency: string
  ): Invoice {
    const convertedAmount = this.convertCurrency(
      invoice.amount,
      invoice.currency,
      toCurrency
    );

    const { subtotal, tax, total } = this.calculateTax(
      convertedAmount,
      invoice.taxRegion.code
    );

    return {
      amount: convertedAmount,
      currency: toCurrency,
      taxRegion: invoice.taxRegion,
      subtotal,
      tax,
      total,
    };
  }

  validateExchangeRate(from: string, to: string, rate: number): boolean {
    const key = `${from}_${to}`;
    const storedRate = this.exchangeRates.get(key);

    if (!storedRate) return false;

    // Allow 1% variance for rate fluctuations
    const variance = Math.abs((rate - storedRate.rate) / storedRate.rate);
    return variance <= 0.01;
  }

  updateExchangeRate(
    from: string,
    to: string,
    newRate: number
  ): ExchangeRate {
    const rate: ExchangeRate = {
      from,
      to,
      rate: newRate,
      timestamp: new Date(),
    };

    const key = `${from}_${to}`;
    this.exchangeRates.set(key, rate);

    return rate;
  }

  getTaxRegion(code: string): TaxRegion | undefined {
    return this.taxRegions.get(code);
  }

  getExchangeRate(from: string, to: string): ExchangeRate | undefined {
    const key = `${from}_${to}`;
    return this.exchangeRates.get(key);
  }

  calculateTaxReport(invoices: Invoice[]): {
    byRegion: Record<string, { count: number; totalTax: number }>;
    totalTax: number;
    invoiceCount: number;
  } {
    const byRegion: Record<string, { count: number; totalTax: number }> = {};
    let totalTax = 0;

    for (const invoice of invoices) {
      const regionCode = invoice.taxRegion.code;
      if (!byRegion[regionCode]) {
        byRegion[regionCode] = { count: 0, totalTax: 0 };
      }
      byRegion[regionCode].count++;
      byRegion[regionCode].totalTax += invoice.tax;
      totalTax += invoice.tax;
    }

    return {
      byRegion,
      totalTax: Math.round(totalTax * 100) / 100,
      invoiceCount: invoices.length,
    };
  }

  recalculateInvoiceWithNewTaxRate(
    invoice: Invoice,
    newTaxRate: number
  ): Invoice {
    const tax = Math.round((invoice.subtotal * newTaxRate) / 100 * 100) / 100;
    const total = Math.round((invoice.subtotal + tax) * 100) / 100;

    return {
      ...invoice,
      tax,
      total,
      taxRegion: {
        ...invoice.taxRegion,
        taxRate: newTaxRate,
      },
    };
  }
}

describe('Multi-Currency and Tax Compliance Tests', () => {
  let engine: MultiCurrencyEngine;

  beforeEach(() => {
    engine = new MultiCurrencyEngine();
  });

  // ============= Currency Conversion =============
  describe('Currency Conversion', () => {
    it('should convert USD to GBP', () => {
      const converted = engine.convertCurrency(100, 'USD', 'GBP');
      expect(converted).toBe(79);
    });

    it('should convert USD to EUR', () => {
      const converted = engine.convertCurrency(100, 'USD', 'EUR');
      expect(converted).toBe(92);
    });

    it('should convert USD to JPY', () => {
      const converted = engine.convertCurrency(100, 'USD', 'JPY');
      expect(converted).toBeCloseTo(14950, 0);
    });

    it('should convert USD to SGD', () => {
      const converted = engine.convertCurrency(100, 'USD', 'SGD');
      expect(converted).toBe(134);
    });

    it('should convert back to USD', () => {
      const converted = engine.convertCurrency(79, 'GBP', 'USD');
      expect(converted).toBeCloseTo(100, 0);
    });

    it('should handle same currency conversion', () => {
      expect(engine.convertCurrency(100, 'USD', 'USD')).toBe(100);
    });

    it('should round to 2 decimal places', () => {
      const converted = engine.convertCurrency(123.456, 'USD', 'GBP');
      // Should be rounded to 2 decimals
      expect(converted.toString().split('.')[1]?.length || 0).toBeLessThanOrEqual(2);
    });

    it('should handle large amounts', () => {
      const converted = engine.convertCurrency(1_000_000, 'USD', 'GBP');
      expect(converted).toBe(790_000);
    });

    it('should handle small amounts', () => {
      const converted = engine.convertCurrency(0.01, 'USD', 'GBP');
      expect(converted).toBeCloseTo(0.0079, 4);
    });
  });

  // ============= Currency Conversion Helpers =============
  describe('Currency Conversion Helpers', () => {
    it('should convert to USD', () => {
      const usd = engine.convertToUSD(79, 'GBP');
      expect(usd).toBeCloseTo(100, 0);
    });

    it('should convert from USD', () => {
      const gbp = engine.convertFromUSD(100, 'GBP');
      expect(gbp).toBe(79);
    });

    it('should handle USD to USD conversion', () => {
      expect(engine.convertToUSD(100, 'USD')).toBe(100);
      expect(engine.convertFromUSD(100, 'USD')).toBe(100);
    });
  });

  // ============= Tax Calculation by Region =============
  describe('Tax Calculation by Region', () => {
    it('should calculate US sales tax (8.875%)', () => {
      const { subtotal, tax, total } = engine.calculateTax(1000, 'US');
      expect(subtotal).toBe(1000);
      expect(tax).toBe(88.75);
      expect(total).toBe(1088.75);
    });

    it('should calculate UK VAT (20%)', () => {
      const { subtotal, tax, total } = engine.calculateTax(1000, 'GB');
      expect(subtotal).toBe(1000);
      expect(tax).toBe(200);
      expect(total).toBe(1200);
    });

    it('should calculate German VAT (19%)', () => {
      const { subtotal, tax, total } = engine.calculateTax(1000, 'DE');
      expect(subtotal).toBe(1000);
      expect(tax).toBe(190);
      expect(total).toBe(1190);
    });

    it('should calculate French VAT (20%)', () => {
      const { subtotal, tax, total } = engine.calculateTax(1000, 'FR');
      expect(subtotal).toBe(1000);
      expect(tax).toBe(200);
      expect(total).toBe(1200);
    });

    it('should calculate Japan consumption tax (10%)', () => {
      const { subtotal, tax, total } = engine.calculateTax(1000, 'JP');
      expect(subtotal).toBe(1000);
      expect(tax).toBe(100);
      expect(total).toBe(1100);
    });

    it('should calculate Singapore GST (8%)', () => {
      const { subtotal, tax, total } = engine.calculateTax(1000, 'SG');
      expect(subtotal).toBe(1000);
      expect(tax).toBe(80);
      expect(total).toBe(1080);
    });

    it('should calculate Australia GST (10%)', () => {
      const { subtotal, tax, total } = engine.calculateTax(1000, 'AU');
      expect(subtotal).toBe(1000);
      expect(tax).toBe(100);
      expect(total).toBe(1100);
    });

    it('should handle small amounts with correct rounding', () => {
      const { subtotal, tax, total } = engine.calculateTax(0.99, 'US');
      expect(tax).toBeCloseTo(0.09, 2);
      expect(total).toBeCloseTo(1.08, 2);
    });

    it('should handle large amounts', () => {
      const { subtotal, tax, total } = engine.calculateTax(1_000_000, 'GB');
      expect(subtotal).toBe(1_000_000);
      expect(tax).toBe(200_000);
      expect(total).toBe(1_200_000);
    });
  });

  // ============= Invoice Creation =============
  describe('Multi-Currency Invoice Creation', () => {
    it('should create USD invoice with sales tax', () => {
      const invoice = engine.createInvoice(1000, 'USD', 'US');

      expect(invoice.currency).toBe('USD');
      expect(invoice.amount).toBe(1000);
      expect(invoice.subtotal).toBe(1000);
      expect(invoice.tax).toBe(88.75);
      expect(invoice.total).toBe(1088.75);
      expect(invoice.taxRegion.type).toBe('sales_tax');
    });

    it('should create GBP invoice with VAT', () => {
      const invoice = engine.createInvoice(1000, 'GBP', 'GB');

      expect(invoice.currency).toBe('GBP');
      expect(invoice.taxRegion.type).toBe('vat');
      expect(invoice.tax).toBe(200);
    });

    it('should create JPY invoice with consumption tax', () => {
      const invoice = engine.createInvoice(100000, 'JPY', 'JP');

      expect(invoice.currency).toBe('JPY');
      expect(invoice.tax).toBe(10000);
    });

    it('should create SGD invoice with GST', () => {
      const invoice = engine.createInvoice(1000, 'SGD', 'SG');

      expect(invoice.currency).toBe('SGD');
      expect(invoice.taxRegion.type).toBe('gst');
      expect(invoice.tax).toBe(80);
    });
  });

  // ============= Invoice Conversion =============
  describe('Invoice Currency Conversion', () => {
    it('should convert invoice from USD to GBP', () => {
      const usdInvoice = engine.createInvoice(1000, 'USD', 'US');
      const gbpInvoice = engine.convertInvoice(usdInvoice, 'GBP');

      expect(gbpInvoice.currency).toBe('GBP');
      expect(gbpInvoice.amount).toBe(79);
      expect(gbpInvoice.subtotal).toBe(79);
    });

    it('should convert invoice from GBP to USD', () => {
      const gbpInvoice = engine.createInvoice(1000, 'GBP', 'GB');
      const usdInvoice = engine.convertInvoice(gbpInvoice, 'USD');

      expect(usdInvoice.currency).toBe('USD');
      expect(usdInvoice.amount).toBeCloseTo(1265, 0);
    });

    it('should recalculate tax for target currency region', () => {
      const usdInvoice = engine.createInvoice(1000, 'USD', 'US');
      const gbpInvoice = engine.convertInvoice(usdInvoice, 'GBP');

      // Converted amount in GBP = 79
      // UK tax rate = 20%
      // Expected tax = 79 * 0.20 = 15.8
      expect(gbpInvoice.tax).toBeCloseTo(15.8, 1);
    });

    it('should maintain referential integrity across conversions', () => {
      const usdInvoice = engine.createInvoice(1000, 'USD', 'US');
      const gbpInvoice = engine.convertInvoice(usdInvoice, 'GBP');
      const backToUSD = engine.convertInvoice(gbpInvoice, 'USD');

      // Should be close to original (within rounding errors)
      expect(backToUSD.amount).toBeCloseTo(usdInvoice.amount, 0);
    });
  });

  // ============= Exchange Rate Management =============
  describe('Exchange Rate Management', () => {
    it('should validate exchange rates within 1% tolerance', () => {
      const valid = engine.validateExchangeRate('USD', 'GBP', 0.7921);
      expect(valid).toBe(true);
    });

    it('should reject exchange rates outside 1% tolerance', () => {
      const invalid = engine.validateExchangeRate('USD', 'GBP', 0.75);
      expect(invalid).toBe(false);
    });

    it('should return false for unknown currency pair', () => {
      const invalid = engine.validateExchangeRate('ZWL', 'XYZ', 1.0);
      expect(invalid).toBe(false);
    });

    it('should update exchange rates', () => {
      const newRate = engine.updateExchangeRate('USD', 'GBP', 0.82);
      expect(newRate.rate).toBe(0.82);
      expect(newRate.timestamp).toBeInstanceOf(Date);

      // Verify update
      const rate = engine.getExchangeRate('USD', 'GBP');
      expect(rate?.rate).toBe(0.82);
    });

    it('should retrieve exchange rates', () => {
      const rate = engine.getExchangeRate('USD', 'GBP');
      expect(rate).toBeDefined();
      expect(rate?.from).toBe('USD');
      expect(rate?.to).toBe('GBP');
    });
  });

  // ============= Tax Region Management =============
  describe('Tax Region Management', () => {
    it('should retrieve tax regions', () => {
      const us = engine.getTaxRegion('US');
      expect(us?.code).toBe('US');
      expect(us?.taxRate).toBe(8.875);
      expect(us?.type).toBe('sales_tax');
    });

    it('should retrieve all supported regions', () => {
      const regions = ['US', 'GB', 'DE', 'FR', 'JP', 'SG', 'AU'];
      for (const regionCode of regions) {
        expect(engine.getTaxRegion(regionCode)).toBeDefined();
      }
    });

    it('should return undefined for unknown region', () => {
      expect(engine.getTaxRegion('XX')).toBeUndefined();
    });
  });

  // ============= Tax Reporting =============
  describe('Tax Reporting and Compliance', () => {
    it('should generate tax report for single region', () => {
      const invoice = engine.createInvoice(1000, 'USD', 'US');
      const report = engine.calculateTaxReport([invoice]);

      expect(report.totalTax).toBe(88.75);
      expect(report.invoiceCount).toBe(1);
      expect(report.byRegion['US'].count).toBe(1);
      expect(report.byRegion['US'].totalTax).toBe(88.75);
    });

    it('should generate tax report for multiple regions', () => {
      const invoices = [
        engine.createInvoice(1000, 'USD', 'US'),
        engine.createInvoice(1000, 'GBP', 'GB'),
        engine.createInvoice(1000, 'EUR', 'DE'),
      ];

      const report = engine.calculateTaxReport(invoices);

      expect(report.invoiceCount).toBe(3);
      expect(report.byRegion['US'].count).toBe(1);
      expect(report.byRegion['GB'].count).toBe(1);
      expect(report.byRegion['DE'].count).toBe(1);
      expect(report.totalTax).toBeCloseTo(488.75, 0);
    });

    it('should aggregate multiple invoices per region', () => {
      const invoices = [
        engine.createInvoice(500, 'USD', 'US'),
        engine.createInvoice(500, 'USD', 'US'),
        engine.createInvoice(1000, 'GBP', 'GB'),
      ];

      const report = engine.calculateTaxReport(invoices);

      expect(report.byRegion['US'].count).toBe(2);
      expect(report.byRegion['US'].totalTax).toBeCloseTo(88.75, 0);
    });
  });

  // ============= Tax Rate Changes =============
  describe('Tax Rate Changes and Recalculation', () => {
    it('should recalculate invoice with new tax rate', () => {
      const invoice = engine.createInvoice(1000, 'USD', 'US');
      const updated = engine.recalculateInvoiceWithNewTaxRate(invoice, 10);

      expect(updated.subtotal).toBe(1000);
      expect(updated.tax).toBe(100);
      expect(updated.total).toBe(1100);
    });

    it('should preserve subtotal when recalculating', () => {
      const invoice = engine.createInvoice(1000, 'USD', 'US');
      const updated = engine.recalculateInvoiceWithNewTaxRate(invoice, 15);

      expect(updated.subtotal).toBe(invoice.subtotal);
    });

    it('should handle zero tax rate', () => {
      const invoice = engine.createInvoice(1000, 'USD', 'US');
      const updated = engine.recalculateInvoiceWithNewTaxRate(invoice, 0);

      expect(updated.tax).toBe(0);
      expect(updated.total).toBe(1000);
    });

    it('should handle high tax rates', () => {
      const invoice = engine.createInvoice(1000, 'USD', 'US');
      const updated = engine.recalculateInvoiceWithNewTaxRate(invoice, 30);

      expect(updated.tax).toBe(300);
      expect(updated.total).toBe(1300);
    });
  });

  // ============= Complex Scenarios =============
  describe('Complex Multi-Currency and Tax Scenarios', () => {
    it('should handle international invoice workflow', () => {
      // Create invoice in USD for US customer
      const usdInvoice = engine.createInvoice(1000, 'USD', 'US');
      expect(usdInvoice.total).toBeCloseTo(1088.75, 2);

      // Convert for reporting in GBP
      const gbpInvoice = engine.convertInvoice(usdInvoice, 'GBP');
      expect(gbpInvoice.currency).toBe('GBP');
      expect(gbpInvoice.total).toBeGreaterThan(0);

      // Generate tax report
      const report = engine.calculateTaxReport([gbpInvoice]);
      expect(report.invoiceCount).toBe(1);
      expect(report.totalTax).toBeGreaterThan(0);
    });

    it('should calculate total revenue across currencies', () => {
      const invoices = [
        engine.createInvoice(1000, 'USD', 'US'),
        engine.createInvoice(1000, 'GBP', 'GB'),
        engine.createInvoice(100000, 'JPY', 'JP'),
        engine.createInvoice(1000, 'SGD', 'SG'),
      ];

      // Convert all to USD for reporting
      const totalsInUSD = invoices.map((inv) => {
        const usdAmount = engine.convertToUSD(inv.total, inv.currency);
        return usdAmount;
      });

      const totalRevenue = totalsInUSD.reduce((a, b) => a + b, 0);
      expect(totalRevenue).toBeGreaterThan(0);
    });

    it('should handle tax exempt transactions', () => {
      const invoice = engine.createInvoice(1000, 'USD', 'US');
      const exempt = engine.recalculateInvoiceWithNewTaxRate(invoice, 0);

      expect(exempt.tax).toBe(0);
      expect(exempt.total).toBe(1000);
    });

    it('should maintain audit trail across currency conversions', () => {
      const original = engine.createInvoice(1000, 'USD', 'US');
      const step1 = engine.convertInvoice(original, 'GBP');
      const step2 = engine.convertInvoice(step1, 'EUR');

      expect(original.currency).toBe('USD');
      expect(step1.currency).toBe('GBP');
      expect(step2.currency).toBe('EUR');
    });
  });

  // ============= Edge Cases =============
  describe('Edge Cases and Error Handling', () => {
    it('should handle zero amount invoices', () => {
      const invoice = engine.createInvoice(0, 'USD', 'US');
      expect(invoice.total).toBe(0);
    });

    it('should handle very small amounts', () => {
      const invoice = engine.createInvoice(0.01, 'USD', 'US');
      expect(invoice.tax).toBeGreaterThanOrEqual(0);
    });

    it('should handle very large amounts', () => {
      const invoice = engine.createInvoice(1_000_000_000, 'USD', 'US');
      expect(invoice.total).toBeGreaterThan(invoice.amount);
    });

    it('should throw error for invalid tax region', () => {
      expect(() => {
        engine.calculateTax(1000, 'XX');
      }).toThrow();
    });

    it('should throw error for missing exchange rate', () => {
      expect(() => {
        engine.convertCurrency(100, 'ZZZ', 'XXX');
      }).toThrow();
    });
  });
});

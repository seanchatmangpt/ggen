/**
 * Type Definitions for Monetization Test Suite
 *
 * Comprehensive type definitions for all monetization components
 * including customers, value definitions, billing, payments, and disputes.
 */

// ============= Customer Types =============
export interface Customer {
  id: string;
  name: string;
  email: string;
  company: string;
  country: string;
  status: 'active' | 'inactive' | 'suspended' | 'canceled';
  createdAt: Date;
  billingEmail: string;
  currency: string;
  metadata?: Record<string, any>;
}

// ============= Value Definition Types =============
export interface ValueDefinition {
  id: string;
  customerId: string;
  metric: string; // e.g., "Revenue Generated", "Efficiency Improvement"
  unit: string;
  baseline: number;
  target: number;
  currency: string;
  pricePerUnit: number;
  status: 'draft' | 'active' | 'suspended' | 'completed';
  createdAt: Date;
  startDate: Date;
  endDate?: Date;
}

// ============= Billing Period Types =============
export interface BillingPeriod {
  id: string;
  customerId: string;
  valueDefId: string;
  startDate: Date;
  endDate: Date;
  status: 'active' | 'completed' | 'disputed' | 'refunded';
  initialValue: number;
  currentValue: number;
  createdAt: Date;
}

// ============= Invoice Types =============
export interface InvoiceItem {
  description: string;
  quantity: number;
  unitPrice: number;
  total: number;
  tax?: number;
  subtotal?: number;
}

export interface Invoice {
  id: string;
  customerId: string;
  billingPeriodId?: string;
  amount: number;
  amountCents: number;
  currency: string;
  status: 'draft' | 'issued' | 'paid' | 'refunded' | 'disputed' | 'void';
  issueDate: Date;
  dueDate: Date;
  items: InvoiceItem[];
  paidDate?: Date;
  metadata?: Record<string, any>;
}

// ============= Payment Types =============
export interface PaymentResult {
  success: boolean;
  invoiceId: string;
  transactionId?: string;
  amount: number;
  error?: string;
  timestamp: Date;
}

export interface Payment {
  id: string;
  invoiceId: string;
  amount: number;
  method: string; // 'credit_card', 'bank_transfer', 'ach', etc.
  status: 'pending' | 'completed' | 'failed' | 'refunded';
  timestamp: Date;
  reference: string;
}

// ============= Refund Types =============
export interface Refund {
  id: string;
  paymentId: string;
  invoiceId: string;
  amount: number;
  reason: string;
  status: 'pending' | 'completed' | 'failed';
  timestamp: Date;
}

// ============= Dispute Types =============
export interface Evidence {
  type: 'metric' | 'log' | 'contract' | 'screenshot' | 'email';
  content: string;
  timestamp: Date;
  source: string;
}

export interface DisputeResolution {
  creditAmount: number;
  reason: string;
  decidedBy: string;
}

export interface Dispute {
  id: string;
  invoiceId: string;
  customerId: string;
  claimedValue: number;
  reason: string;
  status: 'open' | 'under_review' | 'evidence_requested' | 'resolved' | 'rejected';
  evidence: Evidence[];
  createdAt: Date;
  resolvedAt?: Date;
  resolution?: DisputeResolution;
}

// ============= Fraud Detection Types =============
export interface AnomalySignal {
  type: string;
  severity: 'low' | 'medium' | 'high' | 'critical';
  description: string;
  metric?: ValueMetric;
}

export interface ValueMetric {
  id: string;
  timestamp: Date;
  value: number;
  source: string;
  signature?: string;
  publicKey?: string;
}

// ============= Tax Types =============
export interface TaxRegion {
  code: string;
  name: string;
  taxRate: number;
  type: 'sales_tax' | 'vat' | 'gst' | 'consumption_tax';
}

export interface ExchangeRate {
  from: string;
  to: string;
  rate: number;
  timestamp: Date;
}

// ============= Audit Types =============
export interface AuditEntry {
  timestamp: Date;
  action: string;
  amount: number;
  actor: string;
  reason?: string;
}

// ============= Performance Types =============
export interface PerformanceMetrics {
  operationCount: number;
  totalTime: number;
  avgTime: number;
  maxTime: number;
  minTime: number;
  throughput: number; // ops/second
}

export interface SLAConfig {
  maxInvoiceGenerationTime: number; // milliseconds
  maxPaymentProcessingTime: number;
  maxValueCalculationTime: number;
  targetThroughput: number; // events per second
}

// ============= Reporting Types =============
export interface TaxReport {
  byRegion: Record<string, { count: number; totalTax: number }>;
  totalTax: number;
  invoiceCount: number;
  period: { start: Date; end: Date };
}

export interface RevenueReport {
  totalRevenue: number;
  totalTax: number;
  netRevenue: number;
  invoiceCount: number;
  paidInvoiceCount: number;
  disputedInvoiceCount: number;
  averageInvoiceAmount: number;
  currency: string;
}

export interface ComplianceReport {
  totalInvoices: number;
  fullyPaidCount: number;
  partiallyPaidCount: number;
  overdueCount: number;
  disputedCount: number;
  taxComplianceScore: number; // 0-100
  fraudDetectionScore: number; // 0-100
  auditTrailComplete: boolean;
}

// ============= Error Types =============
export class MonetizationError extends Error {
  constructor(
    public code: string,
    message: string,
    public statusCode: number = 500
  ) {
    super(message);
    this.name = 'MonetizationError';
  }
}

export class ValidationError extends MonetizationError {
  constructor(message: string) {
    super('VALIDATION_ERROR', message, 400);
    this.name = 'ValidationError';
  }
}

export class PaymentError extends MonetizationError {
  constructor(message: string) {
    super('PAYMENT_ERROR', message, 402);
    this.name = 'PaymentError';
  }
}

export class FraudDetectedError extends MonetizationError {
  constructor(message: string, public signals: AnomalySignal[] = []) {
    super('FRAUD_DETECTED', message, 403);
    this.name = 'FraudDetectedError';
  }
}

export class DisputeError extends MonetizationError {
  constructor(message: string) {
    super('DISPUTE_ERROR', message, 409);
    this.name = 'DisputeError';
  }
}

// ============= Configuration Types =============
export interface MonetizationConfig {
  enableFraudDetection: boolean;
  enableAuditTrail: boolean;
  defaultCurrency: string;
  supportedCurrencies: string[];
  supportedTaxRegions: string[];
  invoiceGracePeriodDays: number;
  disputeTimeWindowDays: number;
  sla: SLAConfig;
}

export interface BillingConfig {
  billingCycle: 'monthly' | 'quarterly' | 'annually' | 'custom';
  autoRetryFailedPayments: boolean;
  maxRetries: number;
  retryIntervalMinutes: number;
  gracePeriodDays: number;
}

// ============= Request/Response Types =============
export interface CreateCustomerRequest {
  name: string;
  email: string;
  company: string;
  country: string;
}

export interface DefineValueRequest {
  customerId: string;
  metric: string;
  unit: string;
  baseline: number;
  target: number;
  currency: string;
}

export interface ProcessPaymentRequest {
  invoiceId: string;
  amount: number;
  method: string;
  idempotencyKey?: string;
}

export interface CreateDisputeRequest {
  invoiceId: string;
  customerId: string;
  claimedValue: number;
  reason: string;
  evidence?: Evidence[];
}

// ============= Constants =============
export const VALID_CURRENCIES = [
  'USD',
  'GBP',
  'EUR',
  'JPY',
  'SGD',
  'AUD',
  'CAD',
  'CHF',
  'CNY',
  'INR',
];

export const VALID_TAX_REGIONS = [
  'US',
  'GB',
  'DE',
  'FR',
  'JP',
  'SG',
  'AU',
  'CA',
  'CH',
  'CN',
  'IN',
];

export const INVOICE_STATUSES = [
  'draft',
  'issued',
  'paid',
  'refunded',
  'disputed',
  'void',
] as const;

export const PAYMENT_STATUSES = ['pending', 'completed', 'failed', 'refunded'] as const;

export const DISPUTE_STATUSES = [
  'open',
  'under_review',
  'evidence_requested',
  'resolved',
  'rejected',
] as const;

export const FRAUD_SEVERITY_LEVELS = ['low', 'medium', 'high', 'critical'] as const;

// ============= Utilities =============
export function isValidCurrency(currency: string): boolean {
  return VALID_CURRENCIES.includes(currency);
}

export function isValidTaxRegion(region: string): boolean {
  return VALID_TAX_REGIONS.includes(region);
}

export function isValidInvoiceStatus(status: string): boolean {
  return INVOICE_STATUSES.includes(status as any);
}

export function formatCurrency(amount: number, currency: string): string {
  const symbols: Record<string, string> = {
    USD: '$',
    GBP: '£',
    EUR: '€',
    JPY: '¥',
    SGD: 'S$',
    AUD: 'A$',
  };

  const symbol = symbols[currency] || currency;
  return `${symbol}${amount.toFixed(2)}`;
}

export function calculateDaysUntilDue(dueDate: Date): number {
  const now = new Date();
  const diff = dueDate.getTime() - now.getTime();
  return Math.ceil(diff / (1000 * 60 * 60 * 24));
}

export function isInvoiceOverdue(dueDate: Date): boolean {
  return new Date() > dueDate;
}

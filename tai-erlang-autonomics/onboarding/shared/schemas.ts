/**
 * Zod schemas for runtime validation
 * Used on both frontend and backend
 */

import { z } from 'zod';
import * as Types from './types';

// Customer Schemas
export const customerSchema = z.object({
  id: z.string().uuid(),
  name: z.string().min(1).max(255),
  email: z.string().email(),
  status: z.enum([
    Types.CustomerStatus.ONBOARDING,
    Types.CustomerStatus.VALUE_DEFINITION,
    Types.CustomerStatus.APPROVAL_PENDING,
    Types.CustomerStatus.APPROVED,
    Types.CustomerStatus.GO_LIVE_READY,
    Types.CustomerStatus.LIVE,
    Types.CustomerStatus.PAUSED,
  ]),
  createdAt: z.date(),
  updatedAt: z.date(),
  setupSteps: z.array(z.any()),
  valueDefinitions: z.array(z.any()).optional(),
  approvals: z.array(z.any()).optional(),
  lastValueCalculation: z.any().optional(),
});

export const createCustomerSchema = z.object({
  name: z.string().min(1).max(255),
  email: z.string().email(),
});

// Setup Step Schemas
export const setupStepSchema = z.object({
  step: z.number().int().min(1).max(5),
  title: z.string().min(1),
  description: z.string().min(1),
  status: z.enum([
    Types.SetupStepStatus.NOT_STARTED,
    Types.SetupStepStatus.IN_PROGRESS,
    Types.SetupStepStatus.COMPLETED,
    Types.SetupStepStatus.FAILED,
  ]),
  validationUrl: z.string().url(),
  completedAt: z.date().optional(),
  error: z.string().optional(),
});

export const updateSetupStepSchema = z.object({
  status: z.enum([
    Types.SetupStepStatus.IN_PROGRESS,
    Types.SetupStepStatus.COMPLETED,
    Types.SetupStepStatus.FAILED,
  ]),
  error: z.string().optional(),
});

// Value Metric Schemas
export const valueMetricSchema = z.object({
  id: z.string().uuid(),
  name: z.string().min(1).max(255),
  unit: z.string().min(1),
  description: z.string().min(1),
  formula: z.string().optional(),
  dataSource: z.string().min(1),
  refreshInterval: z.number().int().positive(),
  weights: z.record(z.number()).optional(),
});

export const createValueMetricSchema = z.object({
  name: z.string().min(1).max(255),
  unit: z.string().min(1),
  description: z.string().min(1),
  formula: z.string().optional(),
  dataSource: z.string().min(1),
  refreshInterval: z.number().int().positive(),
  weights: z.record(z.number()).optional(),
});

// Value Definition Schemas
export const valueDefinitionSchema = z.object({
  id: z.string().uuid(),
  customerId: z.string().uuid(),
  name: z.string().min(1).max(255),
  description: z.string().min(1),
  metrics: z.array(valueMetricSchema),
  baselineValue: z.number().nonnegative(),
  targetValue: z.number().positive(),
  scoringModel: z.enum(['LINEAR', 'EXPONENTIAL', 'CUSTOM']),
  customFormula: z.string().optional(),
  status: z.enum(['DRAFT', 'ACTIVE', 'ARCHIVED']),
  createdAt: z.date(),
  updatedAt: z.date(),
  validatedAt: z.date().optional(),
});

export const createValueDefinitionSchema = z.object({
  name: z.string().min(1).max(255),
  description: z.string().min(1),
  metrics: z.array(createValueMetricSchema).min(1),
  baselineValue: z.number().nonnegative(),
  targetValue: z.number().positive(),
  scoringModel: z.enum(['LINEAR', 'EXPONENTIAL', 'CUSTOM']),
  customFormula: z.string().optional(),
});

export const updateValueDefinitionSchema = createValueDefinitionSchema.extend({
  status: z.enum(['DRAFT', 'ACTIVE', 'ARCHIVED']).optional(),
});

// Value Calculation Schemas
export const valueCalculationSchema = z.object({
  id: z.string().uuid(),
  customerId: z.string().uuid(),
  valueDefinitionId: z.string().uuid(),
  timestamp: z.date(),
  period: z.enum(['HOURLY', 'DAILY', 'WEEKLY', 'MONTHLY']),
  metricValues: z.record(z.number()),
  calculatedValue: z.number().nonnegative(),
  previousValue: z.number().nonnegative(),
  changePercentage: z.number(),
  receiptHash: z.string(),
  receiptSignature: z.string(),
});

// Receipt Schemas
export const receiptSchema = z.object({
  id: z.string().uuid(),
  customerId: z.string().uuid(),
  type: z.enum(['SETUP', 'VALUE_CALCULATION', 'APPROVAL', 'GO_LIVE', 'MEASUREMENT']),
  data: z.record(z.any()),
  hash: z.string(),
  signature: z.string(),
  previousHash: z.string().optional(),
  timestamp: z.date(),
  verifiedAt: z.date().optional(),
});

export const receiptValidationSchema = z.object({
  receiptId: z.string().uuid(),
  isValid: z.boolean(),
  chainValid: z.boolean(),
  signatureValid: z.boolean(),
  errors: z.array(z.string()).optional(),
  verifiedAt: z.date(),
});

// Approver Schemas
export const approverSchema = z.object({
  id: z.string().uuid(),
  name: z.string().min(1),
  email: z.string().email(),
  role: z.enum(['CFO', 'CTO', 'FINANCE_MANAGER', 'TECHNICAL_LEAD']),
  status: z.enum(['INVITED', 'ACCEPTED', 'DECLINED']),
});

export const inviteApproverSchema = z.object({
  name: z.string().min(1),
  email: z.string().email(),
  role: z.enum(['CFO', 'CTO', 'FINANCE_MANAGER', 'TECHNICAL_LEAD']),
});

// Approval Schemas
export const approvalSchema = z.object({
  id: z.string().uuid(),
  customerId: z.string().uuid(),
  stage: z.enum(['FINANCE_REVIEW', 'TECHNICAL_REVIEW', 'FINAL_APPROVAL']),
  approvers: z.array(approverSchema),
  status: z.enum(['PENDING', 'APPROVED', 'REJECTED']),
  comments: z.string().optional(),
  createdAt: z.date(),
  updatedAt: z.date(),
  approvedAt: z.date().optional(),
  rejectedAt: z.date().optional(),
});

export const createApprovalSchema = z.object({
  stage: z.enum(['FINANCE_REVIEW', 'TECHNICAL_REVIEW', 'FINAL_APPROVAL']),
  approvers: z.array(inviteApproverSchema).min(1),
});

export const approveSchema = z.object({
  comments: z.string().optional(),
});

export const rejectSchema = z.object({
  reason: z.string().min(1),
  comments: z.string().optional(),
});

// Alert Schemas
export const alertSchema = z.object({
  id: z.string().uuid(),
  customerId: z.string().uuid(),
  type: z.enum(['VALUE_THRESHOLD', 'ANOMALY', 'MEASUREMENT_ERROR', 'APPROVAL_NEEDED']),
  severity: z.enum(['INFO', 'WARNING', 'CRITICAL']),
  message: z.string().min(1),
  data: z.record(z.any()),
  isResolved: z.boolean(),
  createdAt: z.date(),
  resolvedAt: z.date().optional(),
});

// Support Ticket Schemas
export const ticketMessageSchema = z.object({
  id: z.string().uuid(),
  ticketId: z.string().uuid(),
  author: z.string().min(1),
  message: z.string().min(1),
  attachments: z.array(z.string()).optional(),
  createdAt: z.date(),
});

export const supportTicketSchema = z.object({
  id: z.string().uuid(),
  customerId: z.string().uuid(),
  title: z.string().min(1).max(255),
  description: z.string().min(1),
  status: z.enum(['OPEN', 'IN_PROGRESS', 'WAITING_FOR_CUSTOMER', 'RESOLVED', 'CLOSED']),
  priority: z.enum(['LOW', 'MEDIUM', 'HIGH', 'URGENT']),
  category: z.enum(['TECHNICAL', 'FINANCIAL', 'INTEGRATION', 'OTHER']),
  messages: z.array(ticketMessageSchema),
  createdAt: z.date(),
  updatedAt: z.date(),
  resolvedAt: z.date().optional(),
});

export const createSupportTicketSchema = z.object({
  title: z.string().min(1).max(255),
  description: z.string().min(1),
  category: z.enum(['TECHNICAL', 'FINANCIAL', 'INTEGRATION', 'OTHER']),
  priority: z.enum(['LOW', 'MEDIUM', 'HIGH', 'URGENT']),
});

export const addTicketMessageSchema = z.object({
  message: z.string().min(1),
  attachments: z.array(z.string()).optional(),
});

// API Key Schemas
export const apiKeySchema = z.object({
  id: z.string().uuid(),
  customerId: z.string().uuid(),
  name: z.string().min(1).max(255),
  key: z.string(),
  secret: z.string(),
  scopes: z.array(z.string()),
  isActive: z.boolean(),
  lastUsedAt: z.date().optional(),
  createdAt: z.date(),
  expiresAt: z.date().optional(),
});

export const createApiKeySchema = z.object({
  name: z.string().min(1).max(255),
  scopes: z.array(z.string()).min(1),
  expiresAt: z.date().optional(),
});

// Integration Config Schemas
export const integrationConfigSchema = z.object({
  id: z.string().uuid(),
  customerId: z.string().uuid(),
  type: z.enum([
    'SALESFORCE',
    'HUBSPOT',
    'SEGMENT',
    'CUSTOM_WEBHOOK',
    'DATADOG',
    'STRIPE',
  ]),
  name: z.string().min(1).max(255),
  config: z.record(z.any()),
  isActive: z.boolean(),
  lastSyncAt: z.date().optional(),
  createdAt: z.date(),
  updatedAt: z.date(),
});

export const createIntegrationConfigSchema = z.object({
  type: z.enum([
    'SALESFORCE',
    'HUBSPOT',
    'SEGMENT',
    'CUSTOM_WEBHOOK',
    'DATADOG',
    'STRIPE',
  ]),
  name: z.string().min(1).max(255),
  config: z.record(z.any()),
});

// Go-Live Switch Schemas
export const goLiveSwitchRequestSchema = z.object({
  customerId: z.string().uuid(),
  requestedAt: z.date(),
  requestedBy: z.string().min(1),
  safetyChecks: z.record(z.boolean()),
  readinessScore: z.number().min(0).max(100),
  estimatedLiveDate: z.date(),
});

export const initiateGoLiveSchema = z.object({
  estimatedLiveDate: z.date(),
});

// Dashboard Schemas
export const dashboardSummarySchema = z.object({
  customerId: z.string().uuid(),
  status: z.enum([
    Types.CustomerStatus.ONBOARDING,
    Types.CustomerStatus.VALUE_DEFINITION,
    Types.CustomerStatus.APPROVAL_PENDING,
    Types.CustomerStatus.APPROVED,
    Types.CustomerStatus.GO_LIVE_READY,
    Types.CustomerStatus.LIVE,
    Types.CustomerStatus.PAUSED,
  ]),
  setupProgress: z.number().min(0).max(100),
  setupSteps: z.array(setupStepSchema),
  currentValue: z.number().nonnegative(),
  targetValue: z.number().positive(),
  valueChangePercentage: z.number(),
  measurementAccuracy: z.number().min(0).max(100),
  approvalsStatus: z.record(z.boolean()),
  lastMeasurementAt: z.date(),
  nextMeasurementAt: z.date(),
  activeAlerts: z.array(alertSchema),
  openTickets: z.number().nonnegative(),
});

// Type exports
export type Customer = z.infer<typeof customerSchema>;
export type CreateCustomer = z.infer<typeof createCustomerSchema>;
export type ValueDefinition = z.infer<typeof valueDefinitionSchema>;
export type CreateValueDefinition = z.infer<typeof createValueDefinitionSchema>;
export type SupportTicket = z.infer<typeof supportTicketSchema>;
export type CreateSupportTicket = z.infer<typeof createSupportTicketSchema>;
export type Alert = z.infer<typeof alertSchema>;
export type Approval = z.infer<typeof approvalSchema>;
export type DashboardSummary = z.infer<typeof dashboardSummarySchema>;

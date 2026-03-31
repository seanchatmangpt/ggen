/**
 * Shared types for the onboarding platform
 * These types are used across frontend and backend
 */

export enum CustomerStatus {
  ONBOARDING = 'ONBOARDING',
  VALUE_DEFINITION = 'VALUE_DEFINITION',
  APPROVAL_PENDING = 'APPROVAL_PENDING',
  APPROVED = 'APPROVED',
  GO_LIVE_READY = 'GO_LIVE_READY',
  LIVE = 'LIVE',
  PAUSED = 'PAUSED',
}

export enum SetupStepStatus {
  NOT_STARTED = 'NOT_STARTED',
  IN_PROGRESS = 'IN_PROGRESS',
  COMPLETED = 'COMPLETED',
  FAILED = 'FAILED',
}

export interface SetupStep {
  step: number;
  title: string;
  description: string;
  status: SetupStepStatus;
  validationUrl: string;
  completedAt?: Date;
  error?: string;
}

export interface Customer {
  id: string;
  name: string;
  email: string;
  status: CustomerStatus;
  createdAt: Date;
  updatedAt: Date;
  setupSteps: SetupStep[];
  valueDefinitions?: ValueDefinition[];
  approvals?: Approval[];
  lastValueCalculation?: ValueCalculation;
}

export interface ValueMetric {
  id: string;
  name: string;
  unit: string;
  description: string;
  formula?: string;
  dataSource: string;
  refreshInterval: number; // seconds
  weights?: Record<string, number>;
}

export interface ValueDefinition {
  id: string;
  customerId: string;
  name: string;
  description: string;
  metrics: ValueMetric[];
  baselineValue: number;
  targetValue: number;
  scoringModel: 'LINEAR' | 'EXPONENTIAL' | 'CUSTOM';
  customFormula?: string;
  status: 'DRAFT' | 'ACTIVE' | 'ARCHIVED';
  createdAt: Date;
  updatedAt: Date;
  validatedAt?: Date;
}

export interface ValueCalculation {
  id: string;
  customerId: string;
  valueDefinitionId: string;
  timestamp: Date;
  period: 'HOURLY' | 'DAILY' | 'WEEKLY' | 'MONTHLY';
  metricValues: Record<string, number>;
  calculatedValue: number;
  previousValue: number;
  changePercentage: number;
  receiptHash: string;
  receiptSignature: string;
}

export interface Receipt {
  id: string;
  customerId: string;
  type: 'SETUP' | 'VALUE_CALCULATION' | 'APPROVAL' | 'GO_LIVE' | 'MEASUREMENT';
  data: Record<string, any>;
  hash: string;
  signature: string;
  previousHash?: string;
  timestamp: Date;
  verifiedAt?: Date;
}

export interface Approver {
  id: string;
  name: string;
  email: string;
  role: 'CFO' | 'CTO' | 'FINANCE_MANAGER' | 'TECHNICAL_LEAD';
  status: 'INVITED' | 'ACCEPTED' | 'DECLINED';
}

export interface Approval {
  id: string;
  customerId: string;
  stage: 'FINANCE_REVIEW' | 'TECHNICAL_REVIEW' | 'FINAL_APPROVAL';
  approvers: Approver[];
  status: 'PENDING' | 'APPROVED' | 'REJECTED';
  comments?: string;
  createdAt: Date;
  updatedAt: Date;
  approvedAt?: Date;
  rejectedAt?: Date;
}

export interface Alert {
  id: string;
  customerId: string;
  type: 'VALUE_THRESHOLD' | 'ANOMALY' | 'MEASUREMENT_ERROR' | 'APPROVAL_NEEDED';
  severity: 'INFO' | 'WARNING' | 'CRITICAL';
  message: string;
  data: Record<string, any>;
  isResolved: boolean;
  createdAt: Date;
  resolvedAt?: Date;
}

export interface SupportTicket {
  id: string;
  customerId: string;
  title: string;
  description: string;
  status: 'OPEN' | 'IN_PROGRESS' | 'WAITING_FOR_CUSTOMER' | 'RESOLVED' | 'CLOSED';
  priority: 'LOW' | 'MEDIUM' | 'HIGH' | 'URGENT';
  category: 'TECHNICAL' | 'FINANCIAL' | 'INTEGRATION' | 'OTHER';
  messages: TicketMessage[];
  createdAt: Date;
  updatedAt: Date;
  resolvedAt?: Date;
}

export interface TicketMessage {
  id: string;
  ticketId: string;
  author: string;
  message: string;
  attachments?: string[];
  createdAt: Date;
}

export interface DashboardSummary {
  customerId: string;
  status: CustomerStatus;
  setupProgress: number; // 0-100
  setupSteps: SetupStep[];
  currentValue: number;
  targetValue: number;
  valueChangePercentage: number;
  measurementAccuracy: number; // 0-100
  approvalsStatus: Record<string, boolean>;
  lastMeasurementAt: Date;
  nextMeasurementAt: Date;
  activeAlerts: Alert[];
  openTickets: number;
}

export interface ValueTrend {
  timestamp: Date;
  value: number;
  baseline: number;
  target: number;
}

export interface APIKey {
  id: string;
  customerId: string;
  name: string;
  key: string;
  secret: string;
  scopes: string[];
  isActive: boolean;
  lastUsedAt?: Date;
  createdAt: Date;
  expiresAt?: Date;
}

export interface IntegrationConfig {
  id: string;
  customerId: string;
  type: 'SALESFORCE' | 'HUBSPOT' | 'SEGMENT' | 'CUSTOM_WEBHOOK' | 'DATADOG' | 'STRIPE';
  name: string;
  config: Record<string, any>;
  isActive: boolean;
  lastSyncAt?: Date;
  createdAt: Date;
  updatedAt: Date;
}

export interface ReceiptValidation {
  receiptId: string;
  isValid: boolean;
  chainValid: boolean;
  signatureValid: boolean;
  errors?: string[];
  verifiedAt: Date;
}

export interface MeasurementAccuracyFeedback {
  id: string;
  customerId: string;
  valueCalculationId: string;
  accuracy: number; // 0-100
  feedback: string;
  suggestedAdjustment?: Record<string, any>;
  createdAt: Date;
}

export interface GoLiveSwitchRequest {
  customerId: string;
  requestedAt: Date;
  requestedBy: string;
  safetyChecks: Record<string, boolean>;
  readinessScore: number; // 0-100
  estimatedLiveDate: Date;
}

export interface OnboardingConfig {
  maxSetupDays: number;
  maxApprovalDays: number;
  minMeasurementAccuracy: number; // percentage
  requireTechnicalReview: boolean;
  requireFinanceReview: boolean;
  allowedValueDefinitionTypes: string[];
  alertThresholds: Record<string, number>;
}

export interface User {
  id: string;
  email: string;
  name: string;
  role: 'ADMIN' | 'CUSTOMER_ADMIN' | 'FINANCE' | 'TECHNICAL';
  customerId?: string;
  authProvider: 'OAUTH2' | 'JWT';
  lastLoginAt?: Date;
  createdAt: Date;
}

export interface ApiResponse<T> {
  success: boolean;
  data?: T;
  error?: {
    code: string;
    message: string;
    details?: Record<string, any>;
  };
  timestamp: Date;
}

export interface PaginatedResponse<T> {
  items: T[];
  total: number;
  page: number;
  pageSize: number;
  hasMore: boolean;
}

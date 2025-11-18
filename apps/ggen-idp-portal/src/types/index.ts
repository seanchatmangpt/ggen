// IDP Domain Types

export interface User {
  id: string
  username: string
  email: string
  displayName?: string
  avatarUrl?: string
  isActive: boolean
  organizationId: string
  createdAt: string
  updatedAt: string
  roles?: Role[]
}

export interface Organization {
  id: string
  name: string
  slug: string
  description?: string
  logoUrl?: string
  createdAt: string
  updatedAt: string
  members?: User[]
}

export interface Role {
  id: string
  name: string
  description?: string
  organizationId: string
  permissions: Permission[]
  parentRoles?: string[]
  createdAt: string
}

export interface Permission {
  resource: string // "pack", "organization", "marketplace"
  action: string // "read", "write", "delete", "publish"
  constraints?: PermissionConstraint
}

export interface PermissionConstraint {
  ownerOnly?: boolean
  orgOnly?: boolean
  requiresApproval?: boolean
  customRules?: string // CEL expression
}

export interface Session {
  id: string
  userId: string
  accessToken: string
  refreshToken: string
  expiresAt: string
  createdAt: string
  ipAddress?: string
  userAgent?: string
  isRevoked: boolean
}

export interface AuthFlow {
  id: string
  name: string
  description?: string
  organizationId: string
  flowType: 'BasicLogin' | 'OauthCallback' | 'MfaVerification' | 'PasswordReset' | 'SignUp' | 'SsoFlow'
  steps: AuthStep[]
  createdAt: string
}

export interface AuthStep {
  id: string
  name: string
  stepType: StepType
  config: Record<string, any>
  nextSteps: StepTransition[]
  errorHandling?: ErrorHandler
}

export type StepType =
  | 'ValidateInput'
  | 'CheckUserExists'
  | 'VerifyPassword'
  | 'GenerateMfaChallenge'
  | 'SendEmail'
  | 'CreateSession'
  | 'IssueToken'
  | 'LogAudit'
  | 'WebhookCall'

export interface StepTransition {
  condition: string // CEL expression
  targetStepId: string
}

export interface ErrorHandler {
  retryCount: number
  backoffMs: number
  fallbackStep?: string
}

export interface OAuth2Client {
  id: string
  secret: string
  name: string
  organizationId: string
  redirectUris: string[]
  allowedScopes: string[]
  isConfidential: boolean
  createdAt: string
}

export interface AuditLogEntry {
  id: string
  userId?: string
  organizationId: string
  action: string // "login", "logout", "create_user", etc.
  resourceType: string
  resourceId?: string
  changes?: Record<string, any>
  status: 'success' | 'failure' | 'warning'
  errorMessage?: string
  ipAddress?: string
  userAgent?: string
  timestamp: string
}

export interface RelationTuple {
  subject: string // e.g., "user:123" or "role:admin"
  relation: string // e.g., "owner", "editor", "viewer"
  object: string // e.g., "pack:abc", "org:xyz"
  timestamp: string
}

// API Response Types
export interface ApiResponse<T> {
  data?: T
  error?: string
  message?: string
  status: number
}

export interface PaginatedResponse<T> {
  data: T[]
  total: number
  page: number
  pageSize: number
  hasMore: boolean
}

// Editor/UI Types
export interface EditorState {
  content: string
  language: string
  isDirty: boolean
  errors: EditorError[]
}

export interface EditorError {
  line: number
  column: number
  message: string
  severity: 'error' | 'warning' | 'info'
}

export interface AICodeSuggestion {
  id: string
  type: 'completion' | 'refactor' | 'optimize' | 'fix'
  description: string
  code: string
  explanation: string
  confidence: number
}

// 2028 Features - Future-ready types
export interface IdentityCredential {
  id: string
  type: 'passkey' | 'webauthn' | 'did' | 'zkproof'
  publicKey: string
  metadata: Record<string, any>
  isVerified: boolean
  createdAt: string
}

export interface DIDDocument {
  id: string
  subject: string
  publicKeys: Array<{
    id: string
    type: string
    publicKeyPem: string
  }>
  authentication: string[]
  assertionMethod: string[]
  serviceEndpoints: Array<{
    id: string
    type: string
    serviceEndpoint: string
  }>
}

export interface ZeroKnowledgeProof {
  id: string
  type: string // "age", "membership", "balance", etc.
  commitment: string
  witness?: string
  verified: boolean
  timestamp: string
}

export interface AutonomousAgent {
  id: string
  name: string
  type: 'credential_issuer' | 'verifier' | 'delegated_signer'
  publicKey: string
  capabilities: string[]
  isActive: boolean
  createdAt: string
}

// Dashboard Types
export interface DashboardMetrics {
  totalUsers: number
  activeUsers: number
  totalAuthFlows: number
  failedLogins: number
  avgSessionDuration: number
  auditLogCount: number
}

export interface AnalyticsData {
  timestamp: string
  loginAttempts: number
  successfulLogins: number
  failedLogins: number
  sessionCount: number
  avgSessionDuration: number
}

// WebSocket Event Types
export interface WebSocketMessage {
  type: 'auth_flow_update' | 'user_action' | 'system_alert' | 'ai_suggestion'
  data: any
  timestamp: string
}

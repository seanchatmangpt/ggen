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

// ==================== 2028 Features - Decentralized Identity (DIDs) ====================
export interface DIDDocument {
  id: string
  subject: string
  didMethod: 'web' | 'key' | 'ethereum' | 'polygon' | 'solana' | 'swarm'
  publicKeys: Array<{
    id: string
    type: 'Ed25519' | 'X25519' | 'Kyber768' | 'Dilithium3' | 'SPHINCS+' | 'BBS+'
    publicKeyPem: string
    createdAt: string
  }>
  authentication: string[]
  assertionMethod: string[]
  serviceEndpoints: Array<{
    id: string
    type: string
    serviceEndpoint: string
  }>
  verifiableCredentials?: VerifiableCredential[]
}

export interface VerifiableCredential {
  id: string
  type: string[]
  issuer: string
  credentialSubject: Record<string, any>
  issuanceDate: string
  expirationDate?: string
  proof?: {
    type: string
    created: string
    verificationMethod: string
    signatureValue: string
  }
  status?: {
    id: string
    type: 'StatusList2021Entry' | 'RevocationList2021Status'
    statusPurpose: 'revocation' | 'suspension'
    statusListIndex: number
    statusListCredential: string
  }
}

export interface SelectiveDisclosure {
  credentialId: string
  revealedClaims: string[]
  proofType: 'BBS+' | 'ZKP'
  challenge?: string
  proof: string
}

// ==================== 2028 Features - Zero-Knowledge Proofs ====================
export interface ZeroKnowledgeProof {
  id: string
  type: 'AgeInRange' | 'Membership' | 'Balance' | 'Credential' | 'Attribute'
  commitment: string
  witness?: string
  provingSystem: 'Groth16' | 'Plonk' | 'Marlin' | 'STARK' | 'Bulletproof' | 'MimC7'
  circuitName: string
  verified: boolean
  verifiedAt?: string
  timestamp: string
}

export interface ZKProofGenerationRequest {
  type: 'AgeInRange' | 'Membership' | 'Balance' | 'Credential'
  data: Record<string, any>
  provingSystem: string
  circuitName: string
}

export interface ZKProofVerification {
  proofId: string
  isValid: boolean
  verificationTime: number
  publicInputs: Record<string, any>
}

export interface PrivacyPool {
  id: string
  name: string
  size: number
  anonymitySet: string[]
  createdAt: string
  updatedAt: string
}

// ==================== 2028 Features - Autonomous Agents ====================
export interface AutonomousAgent {
  id: string
  name: string
  type: 'CredentialIssuer' | 'Verifier' | 'DelegatedSigner' | 'RiskAssessment'
  did: string
  publicKey: string
  capabilities: AgentCapability[]
  llmModel: 'GPT-4' | 'Claude3' | 'Mistral'
  isActive: boolean
  reputation: AgentReputation
  createdAt: string
  updatedAt: string
}

export interface AgentCapability {
  name: string
  description: string
  parameters: Record<string, any>
  rateLimit: number
}

export interface AgentGoal {
  id: string
  agentId: string
  type: 'Issuing' | 'Verifying' | 'DataExchange' | 'Consensus'
  description: string
  constraints: string[]
  successCriteria: string[]
  status: 'Pending' | 'InProgress' | 'Completed' | 'Failed'
  createdAt: string
}

export interface AgentReasoning {
  id: string
  agentId: string
  reasoningType: 'ChainOfThought' | 'TreeOfThoughts' | 'ReflectionCritique'
  steps: ReasoningStep[]
  finalDecision: {
    type: string
    confidence: number
    explanation: string
  }
  createdAt: string
}

export interface ReasoningStep {
  index: number
  thought: string
  action: string
  observation: string
}

export interface AgentReputation {
  overallScore: number // 0-100
  trustScore: number
  capabilityScore: number
  integrityScore: number
  collaborationScore: number
  eventCount: number
  lastUpdated: string
}

export interface AgentConstitution {
  agentId: string
  rules: AgentRule[]
  governance: 'Autonomous' | 'HumanOversight' | 'SharedGovernance'
  auditRequired: boolean
}

export interface AgentRule {
  id: string
  description: string
  condition: string
  action: string
  enforcementLevel: 'Strict' | 'Soft' | 'Advisory'
  penalty?: number
}

// ==================== 2028 Features - Quantum-Safe Cryptography ====================
export interface QuantumSafeKeyPair {
  id: string
  algorithm: 'Kyber-768' | 'Dilithium-3' | 'Falcon-1024' | 'SPHINCS+'
  publicKey: string
  createdAt: string
  keySize: number
  securityLevel: 1 | 3 | 5
}

export interface HybridKeyPair {
  id: string
  classicalKeyPair: {
    algorithm: 'Ed25519' | 'ECDSA' | 'RSA'
    publicKey: string
  }
  quantumSafeKeyPair: QuantumSafeKeyPair
  createdAt: string
}

export interface KeyMigrationPlan {
  id: string
  organizationId: string
  status: 'Planning' | 'InProgress' | 'Testing' | 'RollingOut' | 'Completed'
  currentPhase: 'GenerateNewKeys' | 'DualSignatures' | 'Migration' | 'PhaseOut' | 'Retire'
  targetAlgorithm: string
  completionPercentage: number
  estimatedCompletionDate: string
  startedAt: string
  updatedAt: string
}

// ==================== 2028 Features - Blockchain Integration ====================
export interface OnChainCredential {
  id: string
  credentialId: string
  network: 'Ethereum' | 'Polygon' | 'Solana' | 'Arbitrum' | 'Optimism'
  contractAddress: string
  tokenId: string
  transactionHash: string
  status: 'Pending' | 'Confirmed' | 'Revoked' | 'Suspended' | 'Expired'
  confirmations: number
  createdAt: string
}

export interface CrossChainCredential {
  id: string
  credentialId: string
  networks: Array<{
    network: string
    contractAddress: string
    tokenId: string
    status: string
  }>
  bridgeAddresses: string[]
  createdAt: string
}

export interface AgentBlockchainWallet {
  id: string
  agentId: string
  network: string
  address: string
  balance: string
  createdAt: string
}

export interface GovernanceProposal {
  id: string
  type: 'AddAgent' | 'RemoveAgent' | 'UpdatePolicy' | 'EmergencyShutdown'
  title: string
  description: string
  proposer: string
  status: 'Pending' | 'Active' | 'Succeeded' | 'Defeated' | 'Executed'
  votesFor: number
  votesAgainst: number
  votesAbstain: number
  deadline: string
  createdAt: string
}

// ==================== 2028 Features - Swarm Coordination ====================
export interface AgentSwarm {
  id: string
  name: string
  agents: AutonomousAgent[]
  governanceModel: 'Consensus' | 'Hierarchical' | 'Peer-to-Peer'
  consensusAlgorithm: 'PBFT' | 'Raft' | 'Gossip' | 'CRDT'
  totalMembers: number
  activeMembers: number
  reputationThreshold: number
  createdAt: string
}

export interface ConsensusProposal {
  id: string
  swarmId: string
  type: 'CredentialIssue' | 'CredentialRevoke' | 'AddAgent' | 'UpdatePolicy'
  title: string
  description: string
  proposer: string
  votes: {
    agentId: string
    vote: 'Approve' | 'Reject' | 'Abstain'
    reason?: string
  }[]
  status: 'Proposed' | 'Voting' | 'Decided' | 'Executed' | 'Failed'
  deadline: string
  createdAt: string
  executedAt?: string
}

export interface SwarmMessage {
  id: string
  swarmId: string
  senderId: string
  recipientId?: string
  type: 'Request' | 'Response' | 'Broadcast' | 'Consensus'
  payload: Record<string, any>
  signature: string
  timestamp: string
}

// ==================== 2028 Features - Advanced Credentials ====================
export interface UnifiedCredential {
  id: string
  type: string
  format: 'JSON-LD' | 'JWT' | 'MDoc' | 'CBOR' | 'ZKProof' | 'Blockchain'
  issuer: string
  credentialSubject: Record<string, any>
  claims: CredentialClaim[]
  issuanceDate: string
  expirationDate?: string
  status: 'Active' | 'Suspended' | 'Revoked' | 'Expired'
  metadata: Record<string, any>
}

export interface CredentialClaim {
  name: string
  value: any
  type: string
  disclosureLevel: 'Public' | 'SemiPrivate' | 'Private' | 'ZKOnly'
}

export interface AttributeBasedCredential {
  id: string
  issuer: string
  attributes: Array<{
    name: string
    value: string
    isPrivate: boolean
  }>
  policy: string // CEL expression
  createdAt: string
}

export interface SelectiveDisclosureRequest {
  credentialId: string
  requestedClaims: string[]
  verifier: string
  reason?: string
  deadline: string
}

export interface CredentialRevocation {
  credentialId: string
  reason: 'Superseded' | 'Compromised' | 'Expired' | 'RequestedByHolder'
  revokedAt: string
  revokedBy: string
}

export interface ChainedCredential {
  id: string
  mainCredentialId: string
  requiredCredentials: string[]
  logic: 'AND' | 'OR'
  policy: string
  verified: boolean
}

// ==================== 2028 Features - Biometric Authentication ====================
export interface BiometricEnrollment {
  id: string
  userId: string
  biometricType: 'Fingerprint' | 'FacialRecognition' | 'VoiceRecognition' | 'IrisRecognition'
  templateHash: string
  status: 'Started' | 'InProgress' | 'Complete' | 'Failed'
  qualityScore: number
  livenessDetected: boolean
  createdAt: string
}

export interface BiometricVerification {
  id: string
  enrollmentId: string
  matchScore: number
  livenessScore: number
  spoofingRisk: 'Low' | 'Medium' | 'High'
  isMatched: boolean
  verifiedAt: string
}

export interface ContinuousAuthentication {
  userId: string
  sessionId: string
  behaviorChecks: Array<{
    timestamp: string
    type: 'Typing' | 'MouseMovement' | 'Scrolling' | 'Gait'
    anomalyScore: number
    flagged: boolean
  }>
  isSessionValid: boolean
}

// ==================== 2028 Features - Reputation System ====================
export interface ReputationScore {
  agentId: string
  overallScore: number
  trustScore: number
  capabilityScore: number
  integrityScore: number
  collaborationScore: number
  historyDays: number
  lastUpdated: string
}

export interface ReputationEvent {
  id: string
  agentId: string
  type: 'SuccessfulTransaction' | 'FailedTransaction' | 'SecurityBreach' | 'Collaboration' | 'Excellence'
  impact: number // -100 to +100
  reason: string
  reporterId?: string
  timestamp: string
}

export interface ReputationIncentive {
  id: string
  agentId: string
  type: 'HighReputationBonus' | 'LoyaltyReward' | 'CollaborationBonus'
  amount: number
  triggeredAt: string
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

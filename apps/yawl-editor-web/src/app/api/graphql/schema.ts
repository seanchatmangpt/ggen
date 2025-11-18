// GraphQL Schema Definition for YAWL Editor

export const graphqlSchema = `
  type Query {
    # Workflow queries
    workflow(id: ID!): Workflow
    workflows(filter: WorkflowFilter, limit: Int, offset: Int): [Workflow!]!
    searchWorkflows(query: String!, limit: Int): [Workflow!]!

    # Node queries
    node(id: ID!): Node
    nodesByType(type: String!, limit: Int): [Node!]!

    # User queries
    currentUser: User
    user(id: ID!): User
    users(limit: Int, offset: Int): [User!]!

    # Analytics queries
    workflowMetrics(workflowId: ID!): WorkflowMetrics
    executionStats(workflowId: ID!, timeRange: TimeRange!): ExecutionStats

    # Collaboration queries
    comments(workflowId: ID!): [Comment!]!
    activityLog(workflowId: ID!, limit: Int): [Activity!]!

    # Optimization queries
    optimizationSuggestions(workflowId: ID!): [OptimizationSuggestion!]!
    costAnalysis(workflowId: ID!): CostAnalysis
  }

  type Mutation {
    # Workflow mutations
    createWorkflow(input: CreateWorkflowInput!): Workflow
    updateWorkflow(id: ID!, input: UpdateWorkflowInput!): Workflow
    deleteWorkflow(id: ID!): Boolean
    cloneWorkflow(id: ID!): Workflow

    # Node mutations
    createNode(workflowId: ID!, input: CreateNodeInput!): Node
    updateNode(id: ID!, input: UpdateNodeInput!): Node
    deleteNode(id: ID!): Boolean

    # Comment mutations
    addComment(workflowId: ID!, input: AddCommentInput!): Comment
    updateComment(id: ID!, content: String!): Comment
    deleteComment(id: ID!): Boolean
    resolveComment(id: ID!): Comment

    # Optimization mutations
    applySuggestion(suggestionId: ID!, workflowId: ID!): Workflow

    # User mutations
    updateUser(id: ID!, input: UpdateUserInput!): User
  }

  type Subscription {
    workflowUpdated(id: ID!): Workflow
    nodeUpdated(id: ID!): Node
    commentAdded(workflowId: ID!): Comment
    userPresence(workflowId: ID!): UserPresence
    cursorMoved(workflowId: ID!): CursorPosition
  }

  # Types
  type Workflow {
    id: ID!
    name: String!
    description: String
    owner: User!
    createdAt: DateTime!
    updatedAt: DateTime!
    nodes: [Node!]!
    edges: [Edge!]!
    metrics: WorkflowMetrics
    status: WorkflowStatus!
    visibility: Visibility!
    tags: [String!]
    version: Int!
  }

  type Node {
    id: ID!
    workflowId: ID!
    type: String!
    name: String!
    position: Position!
    config: JSON!
    inputs: [NodePort!]!
    outputs: [NodePort!]!
    metadata: NodeMetadata!
    createdAt: DateTime!
    updatedAt: DateTime!
  }

  type Edge {
    id: ID!
    source: ID!
    target: ID!
    sourcePort: String!
    targetPort: String!
    metadata: JSON
  }

  type NodePort {
    id: String!
    name: String!
    type: String!
    required: Boolean!
  }

  type Position {
    x: Float!
    y: Float!
  }

  type NodeMetadata {
    complexity: Int!
    estimatedDuration: Int!
    estimatedCost: Float!
    reliability: Float!
  }

  type User {
    id: ID!
    email: String!
    name: String!
    avatar: String
    role: UserRole!
    createdAt: DateTime!
    preferences: UserPreferences!
  }

  type UserPreferences {
    theme: String!
    notifications: Boolean!
    language: String!
  }

  type Comment {
    id: ID!
    workflowId: ID!
    nodeId: ID
    author: User!
    content: String!
    createdAt: DateTime!
    updatedAt: DateTime!
    resolved: Boolean!
    replies: [Comment!]!
  }

  type Activity {
    id: ID!
    workflowId: ID!
    actor: User!
    action: String!
    details: JSON!
    timestamp: DateTime!
  }

  type WorkflowMetrics {
    workflowId: ID!
    totalNodes: Int!
    totalEdges: Int!
    maxDepth: Int!
    averageNodeComplexity: Float!
    estimatedCost: Float!
    estimatedDuration: Int!
    reliabilityScore: Float!
    securityScore: Float!
  }

  type ExecutionStats {
    workflowId: ID!
    totalExecutions: Int!
    successfulExecutions: Int!
    failedExecutions: Int!
    averageExecutionTime: Int!
    peakExecutionTime: Int!
    successRate: Float!
    errorRate: Float!
    timeRange: TimeRange!
  }

  type OptimizationSuggestion {
    id: ID!
    workflowId: ID!
    type: SuggestionType!
    severity: Severity!
    title: String!
    description: String!
    recommendation: String!
    estimatedImpact: String!
    appliedCount: Int
  }

  type CostAnalysis {
    workflowId: ID!
    currentCost: Float!
    projectedMonthly: Float!
    optimizationPotential: Float!
    recommendations: [String!]!
  }

  type UserPresence {
    workflowId: ID!
    users: [User!]!
  }

  type CursorPosition {
    userId: ID!
    userName: String!
    position: Position!
    color: String!
  }

  # Input Types
  input CreateWorkflowInput {
    name: String!
    description: String
    tags: [String!]
    visibility: Visibility!
  }

  input UpdateWorkflowInput {
    name: String
    description: String
    tags: [String!]
    visibility: Visibility
  }

  input CreateNodeInput {
    type: String!
    name: String!
    position: PositionInput!
    config: JSON!
  }

  input UpdateNodeInput {
    name: String
    position: PositionInput
    config: JSON
  }

  input PositionInput {
    x: Float!
    y: Float!
  }

  input AddCommentInput {
    nodeId: ID
    content: String!
  }

  input UpdateUserInput {
    name: String
    email: String
    preferences: UserPreferencesInput
  }

  input UserPreferencesInput {
    theme: String
    notifications: Boolean
    language: String
  }

  input WorkflowFilter {
    owner: ID
    status: WorkflowStatus
    tags: [String!]
  }

  input TimeRange {
    start: DateTime!
    end: DateTime!
  }

  # Enums
  enum WorkflowStatus {
    DRAFT
    ACTIVE
    ARCHIVED
    TESTING
  }

  enum UserRole {
    ADMIN
    EDITOR
    VIEWER
    GUEST
  }

  enum Visibility {
    PRIVATE
    TEAM
    PUBLIC
  }

  enum SuggestionType {
    PERFORMANCE
    RELIABILITY
    COST
    SECURITY
    BEST_PRACTICE
  }

  enum Severity {
    CRITICAL
    HIGH
    MEDIUM
    LOW
  }

  # Scalar Types
  scalar DateTime
  scalar JSON
`

export interface GraphQLContext {
  userId: string
  user: any
  permissions: string[]
}

export interface GraphQLError {
  message: string
  code: string
  extensions?: Record<string, any>
}

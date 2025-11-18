/**
 * Backstage IDP - Types and Data Models
 * Comprehensive type definitions for the developer platform
 */

import { z } from 'zod'

// ============================================================================
// Component Catalog Types
// ============================================================================

export interface ComponentMetadata {
  id: string
  name: string
  type: 'service' | 'library' | 'template' | 'plugin'
  description: string
  owner: string
  repo: string
  language: string
  framework: string
  version: string
  status: 'active' | 'deprecated' | 'experimental'
  tags: string[]
  documentation?: string
  apiDocs?: string
  dependencies: string[]
  team: string
  createdAt: Date
  updatedAt: Date
  stars: number
  downloads: number
  healthScore: number
}

export interface ServiceEntity {
  id: string
  name: string
  namespace: string
  title: string
  description: string
  apiVersion: string
  kind: 'Service' | 'API' | 'Library'
  metadata: {
    annotations?: Record<string, string>
    labels?: Record<string, string>
  }
  spec: {
    type: string
    lifecycle: 'production' | 'staging' | 'development'
    owner: string
    providesApis?: string[]
    consumesApis?: string[]
    domain?: string
  }
}

export interface TemplateEntity {
  id: string
  name: string
  title: string
  description: string
  type: 'service' | 'library' | 'website' | 'monorepo'
  owner: string
  repoUrl: string
  tags: string[]
  steps: TemplateStep[]
}

export interface TemplateStep {
  id: string
  title: string
  description: string
  action: string
  input?: Record<string, string>
  output?: Record<string, string>
}

// ============================================================================
// API Management Types
// ============================================================================

export interface APIDefinition {
  id: string
  name: string
  version: string
  description: string
  owner: string
  baseUrl: string
  spec: 'openapi' | 'asyncapi' | 'graphql'
  specUrl: string
  status: 'active' | 'deprecated' | 'planning'
  consumers: string[]
  providers: string[]
  tags: string[]
  rating: number
}

// ============================================================================
// Deployment & Pipeline Types
// ============================================================================

export interface DeploymentPipeline {
  id: string
  name: string
  serviceId: string
  environment: 'dev' | 'staging' | 'production'
  status: 'success' | 'failed' | 'running' | 'pending'
  stages: PipelineStage[]
  createdAt: Date
  startedAt?: Date
  completedAt?: Date
  triggeredBy: string
  artifacts: PipelineArtifact[]
}

export interface PipelineStage {
  name: string
  status: 'pending' | 'running' | 'success' | 'failed' | 'skipped'
  duration: number
  logs: string
  startedAt: Date
  completedAt?: Date
}

export interface PipelineArtifact {
  id: string
  name: string
  type: string
  size: number
  url: string
  checksum: string
}

// ============================================================================
// Team & Ownership Types
// ============================================================================

export interface TeamInfo {
  id: string
  name: string
  slug: string
  description: string
  owner: string
  members: TeamMember[]
  services: string[]
  repos: string[]
  createdAt: Date
  email?: string
  slackChannel?: string
}

export interface TeamMember {
  userId: string
  userName: string
  role: 'admin' | 'maintainer' | 'developer'
  joinedAt: Date
}

// ============================================================================
// Documentation & Knowledge Base
// ============================================================================

export interface DocumentationPage {
  id: string
  title: string
  path: string
  content: string
  markdown: string
  author: string
  lastUpdated: Date
  tags: string[]
  relatedPages: string[]
  viewCount: number
}

export interface RunbookTemplate {
  id: string
  title: string
  description: string
  steps: RunbookStep[]
  owner: string
  tags: string[]
  createdAt: Date
}

export interface RunbookStep {
  title: string
  description: string
  action: string
  parameters?: Record<string, string>
  expectedOutput?: string
  troubleshooting?: string
}

// ============================================================================
// Search & Discovery Types
// ============================================================================

export interface CatalogEntry {
  id: string
  kind: string
  name: string
  namespace: string
  title: string
  description: string
  metadata: Record<string, any>
  spec: Record<string, any>
  relationTo: string[]
  relationFrom: string[]
}

// ============================================================================
// Zod Validation Schemas
// ============================================================================

export const ComponentMetadataSchema = z.object({
  id: z.string().min(1),
  name: z.string().min(1),
  type: z.enum(['service', 'library', 'template', 'plugin']),
  description: z.string().min(10),
  owner: z.string().min(1),
  repo: z.string().url(),
  language: z.string(),
  framework: z.string(),
  version: z.string().regex(/^\d+\.\d+\.\d+$/),
  status: z.enum(['active', 'deprecated', 'experimental']),
  tags: z.array(z.string()),
  documentation: z.string().url().optional(),
  dependencies: z.array(z.string()),
  team: z.string(),
  healthScore: z.number().min(0).max(100),
})

export const ServiceEntitySchema = z.object({
  id: z.string().min(1),
  name: z.string().min(1),
  namespace: z.string(),
  title: z.string(),
  description: z.string(),
  spec: z.object({
    type: z.string(),
    lifecycle: z.enum(['production', 'staging', 'development']),
    owner: z.string(),
  }),
})

export const DeploymentPipelineSchema = z.object({
  id: z.string().min(1),
  name: z.string().min(1),
  serviceId: z.string().min(1),
  environment: z.enum(['dev', 'staging', 'production']),
  status: z.enum(['success', 'failed', 'running', 'pending']),
  stages: z.array(z.object({
    name: z.string(),
    status: z.enum(['pending', 'running', 'success', 'failed', 'skipped']),
    duration: z.number(),
  })),
  triggeredBy: z.string(),
})

export type CreateComponentInput = z.infer<typeof ComponentMetadataSchema>
export type CreateServiceInput = z.infer<typeof ServiceEntitySchema>
export type CreateDeploymentInput = z.infer<typeof DeploymentPipelineSchema>

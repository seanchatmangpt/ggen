// Core Domain Types

export interface User {
  id: string
  email: string
  name: string
  avatar?: string
  roles: Role[]
  teams: Team[]
  createdAt: Date
}

export type Role = 'admin' | 'owner' | 'member' | 'viewer'

export interface Team {
  id: string
  name: string
  description: string
  avatar?: string
  members: TeamMember[]
  owner: User
  services: Service[]
  createdAt: Date
}

export interface TeamMember {
  id: string
  user: User
  role: Role
  joinedAt: Date
}

export interface Service {
  id: string
  name: string
  description: string
  owner: Team
  type: 'backend' | 'frontend' | 'library' | 'tool'
  language: string[]
  repository: {
    url: string
    branch: string
  }
  status: 'active' | 'deprecated' | 'beta'
  dependencies: string[]
  deployments: Deployment[]
  monitoring: ServiceHealth
  tags: string[]
  createdAt: Date
  updatedAt: Date
}

export interface ServiceHealth {
  uptime: number
  errorRate: number
  latency: number
  lastChecked: Date
  status: 'healthy' | 'warning' | 'critical'
}

export interface Deployment {
  id: string
  environment: 'dev' | 'staging' | 'production'
  version: string
  status: 'pending' | 'running' | 'completed' | 'failed'
  deployedAt: Date
  deployedBy: User
}

export interface Template {
  id: string
  name: string
  description: string
  category: string
  language: string
  framework: string
  thumbnail?: string
  version: string
  rating: number
  downloads: number
  parameters: TemplateParameter[]
  tags: string[]
  createdAt: Date
}

export interface TemplateParameter {
  id: string
  name: string
  description: string
  type: 'text' | 'select' | 'boolean' | 'number'
  required: boolean
  default?: any
  options?: Array<{ label: string; value: any }>
}

export interface Documentation {
  id: string
  title: string
  path: string
  content: string
  service?: Service
  tags: string[]
  viewCount: number
  published: boolean
  lastUpdated: Date
}

export interface Notification {
  id: string
  userId: string
  title: string
  message: string
  type: 'info' | 'success' | 'warning' | 'error'
  read: boolean
  createdAt: Date
}

export interface Permission {
  resource: string
  action: 'create' | 'read' | 'update' | 'delete'
  role: Role
}

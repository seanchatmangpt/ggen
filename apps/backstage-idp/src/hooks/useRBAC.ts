import { useState, useCallback } from "react"

export type UserRole = "admin" | "editor" | "viewer" | "guest"
export type ResourceType = "service" | "template" | "workflow" | "documentation" | "team"
export type Action = "read" | "create" | "update" | "delete" | "publish" | "approve" | "manage"

export interface Permission {
  id: string
  role: UserRole
  resource: ResourceType
  action: Action
  conditions?: {
    ownership?: boolean
    teamMembership?: boolean
    tags?: string[]
  }
}

export interface Role {
  id: UserRole
  name: string
  description: string
  permissions: Permission[]
  priority: number
}

export interface User {
  id: string
  name: string
  email: string
  role: UserRole
  teams: string[]
  isOwner?: boolean
}

export interface AuditLog {
  id: string
  timestamp: Date
  userId: string
  userName: string
  action: Action
  resource: {
    type: ResourceType
    id: string
    name: string
  }
  changes?: {
    before: any
    after: any
  }
  status: "success" | "failure"
  reason?: string
  ipAddress?: string
  userAgent?: string
}

const defaultRoles: Role[] = [
  {
    id: "admin",
    name: "Administrator",
    description: "Full access to all resources and settings",
    priority: 100,
    permissions: [
      {
        id: "admin-all",
        role: "admin",
        resource: "service",
        action: "manage",
      },
      {
        id: "admin-all-template",
        role: "admin",
        resource: "template",
        action: "manage",
      },
      {
        id: "admin-all-team",
        role: "admin",
        resource: "team",
        action: "manage",
      },
    ],
  },
  {
    id: "editor",
    name: "Editor",
    description: "Can create and edit resources within teams",
    priority: 50,
    permissions: [
      {
        id: "editor-create",
        role: "editor",
        resource: "service",
        action: "create",
      },
      {
        id: "editor-update-own",
        role: "editor",
        resource: "service",
        action: "update",
        conditions: { ownership: true },
      },
      {
        id: "editor-delete-own",
        role: "editor",
        resource: "service",
        action: "delete",
        conditions: { ownership: true },
      },
    ],
  },
  {
    id: "viewer",
    name: "Viewer",
    description: "Read-only access to resources",
    priority: 25,
    permissions: [
      {
        id: "viewer-read",
        role: "viewer",
        resource: "service",
        action: "read",
      },
      {
        id: "viewer-read-template",
        role: "viewer",
        resource: "template",
        action: "read",
      },
    ],
  },
  {
    id: "guest",
    name: "Guest",
    description: "Limited read-only access",
    priority: 10,
    permissions: [
      {
        id: "guest-read-public",
        role: "guest",
        resource: "service",
        action: "read",
        conditions: { tags: ["public"] },
      },
    ],
  },
]

export function useRBAC(currentUser: User) {
  const [auditLog, setAuditLog] = useState<AuditLog[]>([])
  const [roles, setRoles] = useState<Role[]>(defaultRoles)

  // Check if user has permission
  const hasPermission = useCallback(
    (
      action: Action,
      resource: ResourceType,
      context?: { ownerId?: string; tags?: string[]; teamId?: string }
    ): boolean => {
      const userRole = roles.find((r) => r.id === currentUser.role)
      if (!userRole) return false

      // Admin always has access
      if (currentUser.role === "admin") return true

      // Check if user has the required permission
      const permission = userRole.permissions.find(
        (p) => p.action === action || p.action === "manage"
      )

      if (!permission) return false

      // Check conditions
      if (permission.conditions) {
        // Check ownership
        if (permission.conditions.ownership && context?.ownerId !== currentUser.id) {
          return false
        }

        // Check team membership
        if (permission.conditions.teamMembership && !currentUser.teams.includes(context?.teamId || "")) {
          return false
        }

        // Check tags
        if (permission.conditions.tags) {
          const hasTag = permission.conditions.tags.some((tag) => context?.tags?.includes(tag))
          if (!hasTag) return false
        }
      }

      return true
    },
    [currentUser, roles]
  )

  // Check multiple permissions (requires all)
  const hasAllPermissions = useCallback(
    (
      permissions: Array<{ action: Action; resource: ResourceType; context?: any }>
    ): boolean => {
      return permissions.every((p) => hasPermission(p.action, p.resource, p.context))
    },
    [hasPermission]
  )

  // Check multiple permissions (requires any)
  const hasAnyPermission = useCallback(
    (permissions: Array<{ action: Action; resource: ResourceType; context?: any }>): boolean => {
      return permissions.some((p) => hasPermission(p.action, p.resource, p.context))
    },
    [hasPermission]
  )

  // Log audit entry
  const logAudit = useCallback(
    (
      action: Action,
      resource: { type: ResourceType; id: string; name: string },
      status: "success" | "failure",
      metadata?: {
        changes?: { before: any; after: any }
        reason?: string
        ipAddress?: string
        userAgent?: string
      }
    ) => {
      const entry: AuditLog = {
        id: `audit-${Date.now()}`,
        timestamp: new Date(),
        userId: currentUser.id,
        userName: currentUser.name,
        action,
        resource,
        status,
        ...metadata,
      }

      setAuditLog((prev) => [entry, ...prev].slice(0, 10000)) // Keep last 10000 entries
    },
    [currentUser]
  )

  // Execute action with permission check and audit logging
  const executeWithPermission = useCallback(
    async <T,>(
      action: Action,
      resource: { type: ResourceType; id: string; name: string },
      callback: () => Promise<T>,
      context?: any
    ): Promise<{ success: boolean; data?: T; error?: string }> => {
      // Check permission
      if (!hasPermission(action, resource.type, context)) {
        logAudit(action, resource, "failure", {
          reason: "Permission denied",
        })
        return {
          success: false,
          error: `Permission denied: ${action} on ${resource.type}`,
        }
      }

      try {
        const result = await callback()
        logAudit(action, resource, "success")
        return { success: true, data: result }
      } catch (error) {
        logAudit(action, resource, "failure", {
          reason: error instanceof Error ? error.message : "Unknown error",
        })
        return {
          success: false,
          error: error instanceof Error ? error.message : "Unknown error",
        }
      }
    },
    [hasPermission, logAudit]
  )

  // Get audit logs
  const getAuditLogs = useCallback(
    (filters?: {
      userId?: string
      action?: Action
      resource?: ResourceType
      startTime?: Date
      endTime?: Date
    }): AuditLog[] => {
      return auditLog.filter((log) => {
        if (filters?.userId && log.userId !== filters.userId) return false
        if (filters?.action && log.action !== filters.action) return false
        if (filters?.resource && log.resource.type !== filters.resource) return false
        if (filters?.startTime && log.timestamp < filters.startTime) return false
        if (filters?.endTime && log.timestamp > filters.endTime) return false
        return true
      })
    },
    [auditLog]
  )

  // Get audit summary
  const getAuditSummary = useCallback(
    (days: number = 7): any => {
      const cutoff = new Date(Date.now() - days * 24 * 60 * 60 * 1000)
      const recentLogs = auditLog.filter((log) => log.timestamp > cutoff)

      const summary = {
        totalActions: recentLogs.length,
        successCount: recentLogs.filter((l) => l.status === "success").length,
        failureCount: recentLogs.filter((l) => l.status === "failure").length,
        byUser: new Map<string, number>(),
        byAction: new Map<string, number>(),
        byResource: new Map<string, number>(),
      }

      recentLogs.forEach((log) => {
        summary.byUser.set(log.userName, (summary.byUser.get(log.userName) || 0) + 1)
        summary.byAction.set(log.action, (summary.byAction.get(log.action) || 0) + 1)
        summary.byResource.set(
          log.resource.type,
          (summary.byResource.get(log.resource.type) || 0) + 1
        )
      })

      return {
        ...summary,
        byUser: Object.fromEntries(summary.byUser),
        byAction: Object.fromEntries(summary.byAction),
        byResource: Object.fromEntries(summary.byResource),
      }
    },
    [auditLog]
  )

  // Create or update role
  const defineRole = useCallback((role: Role) => {
    setRoles((prev) => {
      const existing = prev.findIndex((r) => r.id === role.id)
      if (existing >= 0) {
        const updated = [...prev]
        updated[existing] = role
        return updated
      }
      return [...prev, role]
    })
  }, [])

  // Assign role to user
  const assignRole = useCallback((userId: string, role: UserRole) => {
    logAudit("update", { type: "team", id: userId, name: `User ${userId}` }, "success", {
      changes: {
        before: { role: "unknown" },
        after: { role },
      },
    })
  }, [logAudit])

  return {
    // Permission checks
    hasPermission,
    hasAllPermissions,
    hasAnyPermission,

    // Role management
    roles,
    defineRole,
    assignRole,

    // Audit
    auditLog,
    logAudit,
    getAuditLogs,
    getAuditSummary,

    // Execution
    executeWithPermission,
  }
}

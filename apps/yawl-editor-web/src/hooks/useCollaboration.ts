import { useState, useCallback } from "react"

export interface Comment {
  id: string
  author: string
  authorId: string
  content: string
  createdAt: Date
  nodeId?: string
  resolved: boolean
  replies: Comment[]
}

export interface ActivityLog {
  id: string
  timestamp: Date
  actor: string
  action: string
  details: {
    nodeId?: string
    propertyName?: string
    oldValue?: any
    newValue?: any
  }
  changes: number
}

export interface Cursor {
  userId: string
  userName: string
  position: { x: number; y: number }
  color: string
  timestamp: Date
}

export interface Presence {
  userId: string
  userName: string
  email: string
  isOnline: boolean
  lastSeen: Date
}

export function useCollaboration(userId: string, userName: string) {
  const [comments, setComments] = useState<Map<string, Comment[]>>(new Map())
  const [activityLog, setActivityLog] = useState<ActivityLog[]>([])
  const [cursors, setCursors] = useState<Map<string, Cursor>>(new Map())
  const [presenceList, setPresenceList] = useState<Presence[]>([])

  // Add comment to a node
  const addComment = useCallback(
    (nodeId: string, content: string) => {
      const comment: Comment = {
        id: `comment-${Date.now()}`,
        author: userName,
        authorId: userId,
        content,
        createdAt: new Date(),
        nodeId,
        resolved: false,
        replies: [],
      }

      setComments((prev) => {
        const nodeComments = prev.get(nodeId) || []
        return new Map(prev).set(nodeId, [...nodeComments, comment])
      })

      // Log activity
      logActivity(`Added comment on node ${nodeId}`, { nodeId })

      return comment
    },
    [userId, userName]
  )

  // Reply to comment
  const replyToComment = useCallback(
    (nodeId: string, parentCommentId: string, content: string) => {
      const reply: Comment = {
        id: `reply-${Date.now()}`,
        author: userName,
        authorId: userId,
        content,
        createdAt: new Date(),
        nodeId,
        resolved: false,
        replies: [],
      }

      setComments((prev) => {
        const nodeComments = [...(prev.get(nodeId) || [])]
        const addReplyToComment = (comment: Comment): Comment => {
          if (comment.id === parentCommentId) {
            return { ...comment, replies: [...comment.replies, reply] }
          }
          return { ...comment, replies: comment.replies.map(addReplyToComment) }
        }
        return new Map(prev).set(nodeId, nodeComments.map(addReplyToComment))
      })

      logActivity(`Replied to comment on node ${nodeId}`, { nodeId })
      return reply
    },
    [userId, userName]
  )

  // Resolve comment thread
  const resolveComment = useCallback((nodeId: string, commentId: string) => {
    setComments((prev) => {
      const nodeComments = [...(prev.get(nodeId) || [])]
      const markAsResolved = (comment: Comment): Comment => {
        if (comment.id === commentId) {
          return { ...comment, resolved: true }
        }
        return { ...comment, replies: comment.replies.map(markAsResolved) }
      }
      return new Map(prev).set(nodeId, nodeComments.map(markAsResolved))
    })

    logActivity(`Resolved comment on node ${nodeId}`, { nodeId })
  }, [])

  // Log activity
  const logActivity = useCallback(
    (action: string, details: ActivityLog["details"] = {}) => {
      const activityEntry: ActivityLog = {
        id: `activity-${Date.now()}`,
        timestamp: new Date(),
        actor: userName,
        action,
        details,
        changes: 1,
      }

      setActivityLog((prev) => [activityEntry, ...prev].slice(0, 1000)) // Keep last 1000 activities
    },
    [userName]
  )

  // Update cursor position for real-time collaboration
  const updateCursorPosition = useCallback(
    (x: number, y: number) => {
      const colors = ["#FF6B6B", "#4ECDC4", "#45B7D1", "#FFA07A", "#98D8C8"]
      const color = colors[userId.charCodeAt(0) % colors.length]

      setCursors((prev) => {
        const newCursors = new Map(prev)
        newCursors.set(userId, {
          userId,
          userName,
          position: { x, y },
          color,
          timestamp: new Date(),
        })
        return newCursors
      })
    },
    [userId, userName]
  )

  // Broadcast presence
  const broadcastPresence = useCallback(
    (isOnline: boolean) => {
      setPresenceList((prev) => {
        const filtered = prev.filter((p) => p.userId !== userId)
        return [
          ...filtered,
          {
            userId,
            userName,
            email: `${userName}@company.com`,
            isOnline,
            lastSeen: new Date(),
          },
        ]
      })

      logActivity(isOnline ? "Came online" : "Went offline", {})
    },
    [userId, userName]
  )

  // Get comments for a node
  const getNodeComments = useCallback(
    (nodeId: string) => {
      return comments.get(nodeId) || []
    },
    [comments]
  )

  // Get unresolved comments count
  const getUnresolvedCount = useCallback(() => {
    let count = 0
    comments.forEach((nodeComments) => {
      nodeComments.forEach((comment) => {
        if (!comment.resolved) count++
        count += comment.replies.filter((r) => !r.resolved).length
      })
    })
    return count
  }, [comments])

  // Get activity summary
  const getActivitySummary = useCallback(
    (hours: number = 24) => {
      const cutoffTime = new Date(Date.now() - hours * 60 * 60 * 1000)
      const filtered = activityLog.filter((a) => a.timestamp > cutoffTime)

      const byActor = new Map<string, number>()
      filtered.forEach((a) => {
        byActor.set(a.actor, (byActor.get(a.actor) || 0) + 1)
      })

      return {
        totalActivities: filtered.length,
        byActor: Object.fromEntries(byActor),
        timeRange: { start: cutoffTime, end: new Date() },
      }
    },
    [activityLog]
  )

  // Get active collaborators
  const getActiveCollaborators = useCallback(() => {
    return presenceList.filter((p) => p.isOnline)
  }, [presenceList])

  return {
    // Comments
    comments: Object.fromEntries(comments),
    addComment,
    replyToComment,
    resolveComment,
    getNodeComments,
    getUnresolvedCount,

    // Activity
    activityLog,
    logActivity,
    getActivitySummary,

    // Real-time Collaboration
    cursors: Object.fromEntries(cursors),
    updateCursorPosition,
    presenceList,
    broadcastPresence,
    getActiveCollaborators,
  }
}

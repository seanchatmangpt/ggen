'use client'

import { useEffect, useState } from 'react'
import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { ScrollArea } from '@/components/ui/scroll-area'
import { PresenceIndicator } from './presence-indicator'
import type { Presence, CollaborationMessage } from '@/lib/collaboration'

interface SharedWorkspaceProps {
  workspaceName: string
  members: Presence[]
  recentActivity?: CollaborationMessage[]
  maxActivityItems?: number
}

/**
 * Displays shared workspace with active members and recent activity
 */
export function SharedWorkspace({
  workspaceName,
  members,
  recentActivity = [],
  maxActivityItems = 10,
}: SharedWorkspaceProps) {
  const onlineMembers = members.filter((m) => m.online)
  const activityItems = recentActivity.slice(0, maxActivityItems)

  const getActivityIcon = (eventType: string): string => {
    if (eventType.includes('created')) return '✨'
    if (eventType.includes('updated')) return '✏️'
    if (eventType.includes('deleted')) return '🗑️'
    if (eventType.includes('allocated')) return '👤'
    if (eventType.includes('online')) return '🟢'
    if (eventType.includes('offline')) return '⚫'
    return '📝'
  }

  const getActivityColor = (eventType: string): string => {
    if (eventType.includes('deleted')) return 'text-red-600'
    if (eventType.includes('created')) return 'text-green-600'
    if (eventType.includes('updated')) return 'text-blue-600'
    return 'text-slate-600'
  }

  const formatActivityTime = (timestamp: number): string => {
    const now = Date.now()
    const diff = now - timestamp
    const minutes = Math.floor(diff / 60000)
    const hours = Math.floor(minutes / 60)
    const days = Math.floor(hours / 24)

    if (minutes < 1) return 'just now'
    if (minutes < 60) return `${minutes}m ago`
    if (hours < 24) return `${hours}h ago`
    return `${days}d ago`
  }

  return (
    <div className="space-y-4">
      {/* Workspace Header */}
      <Card>
        <CardHeader>
          <div className="flex items-center justify-between">
            <div>
              <CardTitle className="text-lg">{workspaceName}</CardTitle>
              <CardDescription>
                {onlineMembers.length} of {members.length} members online
              </CardDescription>
            </div>
            <Badge variant="outline" className="text-lg py-1 px-3">
              {onlineMembers.length} Active
            </Badge>
          </div>
        </CardHeader>
      </Card>

      {/* Members List */}
      <Card>
        <CardHeader>
          <CardTitle className="text-sm">Team Members</CardTitle>
        </CardHeader>
        <CardContent>
          <ScrollArea className="h-64">
            <div className="space-y-3 pr-4">
              {members.length === 0 ? (
                <p className="text-sm text-slate-500">No members yet</p>
              ) : (
                members.map((member) => (
                  <PresenceIndicator
                    key={member.userId}
                    presence={member}
                    showView={true}
                    size="md"
                  />
                ))
              )}
            </div>
          </ScrollArea>
        </CardContent>
      </Card>

      {/* Recent Activity */}
      {activityItems.length > 0 && (
        <Card>
          <CardHeader>
            <CardTitle className="text-sm">Recent Activity</CardTitle>
          </CardHeader>
          <CardContent>
            <ScrollArea className="h-64">
              <div className="space-y-3 pr-4">
                {activityItems.map((activity) => (
                  <div
                    key={activity.id}
                    className="flex items-start gap-3 pb-3 border-b last:border-0"
                  >
                    <span className="text-xl mt-1">
                      {getActivityIcon(activity.type)}
                    </span>
                    <div className="flex-1 min-w-0">
                      <p className="text-sm font-medium">
                        <span className="font-semibold">
                          {activity.userName}
                        </span>{' '}
                        <span className={getActivityColor(activity.type)}>
                          {activity.type.replace(':', ' ').charAt(0).toUpperCase() +
                            activity.type.slice(1)}
                        </span>
                      </p>
                      <div className="flex items-center gap-2 text-xs text-slate-500">
                        <span className="truncate">{activity.resource}</span>
                        <span>•</span>
                        <span>{formatActivityTime(activity.timestamp)}</span>
                      </div>
                    </div>
                  </div>
                ))}
              </div>
            </ScrollArea>
          </CardContent>
        </Card>
      )}
    </div>
  )
}

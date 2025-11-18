'use client'

import { useEffect, useState } from 'react'
import { Badge } from '@/components/ui/badge'
import type { Presence } from '@/lib/collaboration'

interface PresenceIndicatorProps {
  presence: Presence
  showView?: boolean
  size?: 'sm' | 'md' | 'lg'
}

/**
 * Display user presence status with avatar and details
 */
export function PresenceIndicator({
  presence,
  showView = true,
  size = 'md',
}: PresenceIndicatorProps) {
  const [relative, setRelative] = useState<string>('')

  useEffect(() => {
    const updateRelativeTime = () => {
      const now = Date.now()
      const diff = now - presence.lastSeen
      const seconds = Math.floor(diff / 1000)
      const minutes = Math.floor(seconds / 60)
      const hours = Math.floor(minutes / 60)

      if (seconds < 60) setRelative('just now')
      else if (minutes < 60) setRelative(`${minutes}m ago`)
      else if (hours < 24) setRelative(`${hours}h ago`)
      else setRelative('offline')
    }

    updateRelativeTime()
    const interval = setInterval(updateRelativeTime, 60000)
    return () => clearInterval(interval)
  }, [presence.lastSeen])

  const sizeClasses = {
    sm: 'h-6 w-6 text-xs',
    md: 'h-8 w-8 text-sm',
    lg: 'h-10 w-10 text-base',
  }

  const initials = presence.userName
    .split(' ')
    .map((n) => n[0])
    .join('')
    .toUpperCase()

  return (
    <div className="flex items-center gap-2">
      <div className="relative">
        <div
          className={`${sizeClasses[size]} rounded-full bg-gradient-to-br from-blue-400 to-blue-600 text-white flex items-center justify-center font-semibold flex-shrink-0`}
        >
          {initials}
        </div>
        {presence.online && (
          <div className="absolute bottom-0 right-0 h-2 w-2 rounded-full bg-green-500 border border-white" />
        )}
      </div>

      <div className="flex-1 min-w-0">
        <p className="text-sm font-medium truncate">{presence.userName}</p>
        <div className="flex items-center gap-2">
          <Badge
            variant={presence.online ? 'default' : 'secondary'}
            className="text-xs"
          >
            {presence.online ? 'Online' : 'Offline'}
          </Badge>
          {showView && presence.currentView && (
            <span className="text-xs text-slate-500 truncate">
              Viewing: {presence.currentView}
            </span>
          )}
          {!presence.online && (
            <span className="text-xs text-slate-500">{relative}</span>
          )}
        </div>
      </div>
    </div>
  )
}

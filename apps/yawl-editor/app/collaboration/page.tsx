'use client'

import { useState, useCallback, useEffect } from 'react'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs'
import { Input } from '@/components/ui/input'
import { SharedWorkspace } from '@/components/collaboration/shared-workspace'
import { PresenceIndicator } from '@/components/collaboration/presence-indicator'
import { useCollaboration } from '@/hooks/use-collaboration'
import { Users, MessageSquare, Activity, Share2 } from 'lucide-react'
import type { Presence } from '@/lib/collaboration'

/**
 * Team Collaboration Page
 * Real-time workspace with presence tracking and activity monitoring
 */
export default function CollaborationPage() {
  const [currentUser, setCurrentUser] = useState({ id: 'user-1', name: 'Current User' })
  const [currentView, setCurrentView] = useState('collaboration')
  const [message, setMessage] = useState('')
  const [messages, setMessages] = useState<{ user: string; text: string; timestamp: string }[]>([
    {
      user: 'Sarah Chen',
      text: 'Just completed the invoice workflow optimization',
      timestamp: '10:32 AM',
    },
    {
      user: 'Mike Johnson',
      text: 'Found a bottleneck in the approval process',
      timestamp: '10:28 AM',
    },
    {
      user: 'Alex Rivera',
      text: 'New case CSE-2024-5123 requires team review',
      timestamp: '10:25 AM',
    },
  ])

  // Mock collaboration data
  const [mockMembers] = useState<Presence[]>([
    {
      userId: 'user-1',
      userName: 'Sarah Chen',
      online: true,
      lastSeen: Date.now(),
      currentView: 'Process Designer',
    },
    {
      userId: 'user-2',
      userName: 'Mike Johnson',
      online: true,
      lastSeen: Date.now() - 120000,
      currentView: 'Case Management',
    },
    {
      userId: 'user-3',
      userName: 'Alex Rivera',
      online: true,
      lastSeen: Date.now() - 300000,
      currentView: 'Analytics Dashboard',
    },
    {
      userId: 'user-4',
      userName: 'Jordan Smith',
      online: false,
      lastSeen: Date.now() - 3600000,
      currentView: undefined,
    },
    {
      userId: 'user-5',
      userName: 'Casey Williams',
      online: false,
      lastSeen: Date.now() - 7200000,
      currentView: undefined,
    },
  ])

  const handleSendMessage = useCallback(() => {
    if (message.trim()) {
      const now = new Date()
      const timeStr = now.toLocaleTimeString('en-US', { hour: '2-digit', minute: '2-digit' })
      setMessages((prev) => [
        ...prev,
        {
          user: currentUser.name,
          text: message,
          timestamp: timeStr,
        },
      ])
      setMessage('')
    }
  }, [message, currentUser.name])

  const onlineCount = mockMembers.filter((m) => m.online).length
  const totalCount = mockMembers.length

  return (
    <div className="min-h-screen bg-slate-50 p-4 md:p-8">
      {/* Header */}
      <div className="mb-8">
        <h1 className="text-3xl font-bold mb-2">Team Collaboration</h1>
        <p className="text-slate-600">
          Real-time workspace for team coordination and process management
        </p>
      </div>

      {/* Main Content */}
      <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
        {/* Left Column - Shared Workspace */}
        <div className="lg:col-span-2 space-y-6">
          {/* Workspace Overview */}
          <Card>
            <CardHeader>
              <CardTitle className="flex items-center gap-2">
                <Users className="h-5 w-5" />
                Shared Workspace
              </CardTitle>
              <CardDescription>
                {onlineCount} of {totalCount} team members online
              </CardDescription>
            </CardHeader>
            <CardContent>
              <SharedWorkspace
                workspaceName="Primary Workflow Team"
                members={mockMembers}
                recentActivity={[
                  {
                    id: 'act-1',
                    type: 'process:created',
                    userId: 'user-1',
                    userName: 'Sarah Chen',
                    resource: 'process',
                    resourceId: 'proc-123',
                    data: {},
                    timestamp: Date.now() - 300000,
                    sessionId: 'sess-1',
                  },
                  {
                    id: 'act-2',
                    type: 'case:updated',
                    userId: 'user-2',
                    userName: 'Mike Johnson',
                    resource: 'case',
                    resourceId: 'case-456',
                    data: {},
                    timestamp: Date.now() - 600000,
                    sessionId: 'sess-2',
                  },
                  {
                    id: 'act-3',
                    type: 'workitem:allocated',
                    userId: 'user-3',
                    userName: 'Alex Rivera',
                    resource: 'workitem',
                    resourceId: 'wi-789',
                    data: {},
                    timestamp: Date.now() - 900000,
                    sessionId: 'sess-3',
                  },
                ]}
              />
            </CardContent>
          </Card>

          {/* Tabs for Communication and Activity */}
          <Tabs defaultValue="messages" className="space-y-4">
            <TabsList className="grid w-full grid-cols-3">
              <TabsTrigger value="messages" className="flex gap-2">
                <MessageSquare className="h-4 w-4" />
                Messages
              </TabsTrigger>
              <TabsTrigger value="activity" className="flex gap-2">
                <Activity className="h-4 w-4" />
                Activity Feed
              </TabsTrigger>
              <TabsTrigger value="sharing" className="flex gap-2">
                <Share2 className="h-4 w-4" />
                Sharing
              </TabsTrigger>
            </TabsList>

            {/* Messages Tab */}
            <TabsContent value="messages" className="space-y-4">
              <Card>
                <CardHeader>
                  <CardTitle className="text-base">Team Chat</CardTitle>
                </CardHeader>
                <CardContent className="space-y-4">
                  {/* Message History */}
                  <div className="bg-slate-50 rounded-lg p-4 h-64 overflow-y-auto space-y-3 mb-4">
                    {messages.map((msg, idx) => (
                      <div key={idx} className="flex gap-3">
                        <div className="h-8 w-8 rounded-full bg-gradient-to-br from-blue-400 to-blue-600 text-white text-xs flex items-center justify-center font-semibold flex-shrink-0">
                          {msg.user[0]}
                        </div>
                        <div className="flex-1">
                          <div className="flex items-center justify-between">
                            <p className="font-medium text-sm">{msg.user}</p>
                            <p className="text-xs text-slate-500">{msg.timestamp}</p>
                          </div>
                          <p className="text-sm text-slate-700 mt-1">{msg.text}</p>
                        </div>
                      </div>
                    ))}
                  </div>

                  {/* Message Input */}
                  <div className="flex gap-2">
                    <Input
                      placeholder="Type a message..."
                      value={message}
                      onChange={(e) => setMessage(e.target.value)}
                      onKeyPress={(e) => {
                        if (e.key === 'Enter') handleSendMessage()
                      }}
                    />
                    <Button onClick={handleSendMessage}>Send</Button>
                  </div>
                </CardContent>
              </Card>
            </TabsContent>

            {/* Activity Feed Tab */}
            <TabsContent value="activity" className="space-y-4">
              <Card>
                <CardHeader>
                  <CardTitle className="text-base">Recent Activity</CardTitle>
                </CardHeader>
                <CardContent>
                  <div className="space-y-4">
                    {[
                      {
                        user: 'Sarah Chen',
                        action: 'Created new process',
                        resource: 'Invoice Processing v2',
                        time: '2 hours ago',
                        icon: '✨',
                      },
                      {
                        user: 'Mike Johnson',
                        action: 'Updated case status',
                        resource: 'CSE-2024-5123',
                        time: '1 hour ago',
                        icon: '✏️',
                      },
                      {
                        user: 'Alex Rivera',
                        action: 'Allocated workitem',
                        resource: 'wi-789 to Sarah',
                        time: '45 minutes ago',
                        icon: '👤',
                      },
                      {
                        user: 'Current User',
                        action: 'Joined the workspace',
                        resource: 'Primary Workflow Team',
                        time: '15 minutes ago',
                        icon: '🟢',
                      },
                    ].map((activity, idx) => (
                      <div key={idx} className="flex gap-3 p-3 rounded-lg bg-slate-50">
                        <span className="text-xl">{activity.icon}</span>
                        <div className="flex-1 min-w-0">
                          <p className="text-sm">
                            <span className="font-semibold">{activity.user}</span> {activity.action}
                          </p>
                          <p className="text-xs text-slate-500 mt-1">{activity.resource}</p>
                          <p className="text-xs text-slate-400 mt-1">{activity.time}</p>
                        </div>
                      </div>
                    ))}
                  </div>
                </CardContent>
              </Card>
            </TabsContent>

            {/* Sharing Tab */}
            <TabsContent value="sharing" className="space-y-4">
              <Card>
                <CardHeader>
                  <CardTitle className="text-base">Shared Resources</CardTitle>
                </CardHeader>
                <CardContent>
                  <div className="space-y-3">
                    {[
                      {
                        name: 'Invoice Processing Workflow',
                        sharedBy: 'Sarah Chen',
                        sharedWith: '5 people',
                        type: 'Process',
                      },
                      {
                        name: 'Q4 Compliance Cases',
                        sharedBy: 'Mike Johnson',
                        sharedWith: '8 people',
                        type: 'Dataset',
                      },
                      {
                        name: 'Team Performance Dashboard',
                        sharedBy: 'Alex Rivera',
                        sharedWith: '12 people',
                        type: 'Analytics',
                      },
                    ].map((resource, idx) => (
                      <div
                        key={idx}
                        className="p-3 rounded-lg border flex items-center justify-between"
                      >
                        <div>
                          <p className="font-medium text-sm">{resource.name}</p>
                          <p className="text-xs text-slate-500">
                            Shared by {resource.sharedBy} with {resource.sharedWith}
                          </p>
                        </div>
                        <Badge variant="outline">{resource.type}</Badge>
                      </div>
                    ))}
                  </div>
                </CardContent>
              </Card>
            </TabsContent>
          </Tabs>
        </div>

        {/* Right Column - Team Members */}
        <div className="space-y-6">
          {/* Online Members */}
          <Card>
            <CardHeader>
              <CardTitle className="text-base">Online Members</CardTitle>
            </CardHeader>
            <CardContent className="space-y-3">
              {mockMembers
                .filter((m) => m.online)
                .map((member) => (
                  <PresenceIndicator
                    key={member.userId}
                    presence={member}
                    showView={true}
                    size="md"
                  />
                ))}
            </CardContent>
          </Card>

          {/* Team Directory */}
          <Card>
            <CardHeader>
              <CardTitle className="text-base">Team Directory</CardTitle>
              <CardDescription>
                All {totalCount} team members
              </CardDescription>
            </CardHeader>
            <CardContent className="space-y-2">
              {mockMembers.map((member) => (
                <div
                  key={member.userId}
                  className="flex items-center justify-between p-2 rounded-lg hover:bg-slate-50"
                >
                  <span className="font-medium text-sm">{member.userName}</span>
                  <Badge
                    variant={member.online ? 'default' : 'secondary'}
                    className="text-xs"
                  >
                    {member.online ? 'Online' : 'Offline'}
                  </Badge>
                </div>
              ))}
            </CardContent>
          </Card>

          {/* Workspace Settings */}
          <Card>
            <CardHeader>
              <CardTitle className="text-base">Workspace Settings</CardTitle>
            </CardHeader>
            <CardContent className="space-y-3">
              <Button variant="outline" className="w-full justify-start">
                Invite Members
              </Button>
              <Button variant="outline" className="w-full justify-start">
                Manage Permissions
              </Button>
              <Button variant="outline" className="w-full justify-start">
                Workspace Settings
              </Button>
            </CardContent>
          </Card>
        </div>
      </div>
    </div>
  )
}

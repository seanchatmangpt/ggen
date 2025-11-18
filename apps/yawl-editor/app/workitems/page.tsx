'use client'

import { useState } from 'react'
import { Card } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import type { Workitem, WorkitemStatus } from '@/lib/types'

const mockWorkitems: Workitem[] = [
  {
    id: 'wi-001',
    status: 'Allocated' as WorkitemStatus,
    createdDate: new Date('2024-01-15'),
    caseId: 'case-001',
    taskId: 'task-001',
    taskName: 'Review Application',
    allocatedTo: 'John Smith',
    allocatedToId: 'user-001',
    priority: 4,
    dueDate: new Date('2024-01-20'),
  },
  {
    id: 'wi-002',
    status: 'Executing' as WorkitemStatus,
    createdDate: new Date('2024-01-16'),
    caseId: 'case-001',
    taskId: 'task-002',
    taskName: 'Process Documents',
    allocatedTo: 'Jane Doe',
    allocatedToId: 'user-002',
    priority: 5,
    dueDate: new Date('2024-01-19'),
  },
  {
    id: 'wi-003',
    status: 'Completed' as WorkitemStatus,
    createdDate: new Date('2024-01-10'),
    completedDate: new Date('2024-01-12'),
    caseId: 'case-002',
    taskId: 'task-001',
    taskName: 'Initial Assessment',
    allocatedTo: 'Bob Johnson',
    allocatedToId: 'user-003',
    priority: 3,
  },
]

const statusColors: Record<WorkitemStatus, string> = {
  Created: 'bg-gray-100 text-gray-800',
  Enabled: 'bg-blue-100 text-blue-800',
  Offered: 'bg-indigo-100 text-indigo-800',
  Allocated: 'bg-purple-100 text-purple-800',
  Executing: 'bg-orange-100 text-orange-800',
  Completed: 'bg-green-100 text-green-800',
  Cancelled: 'bg-red-100 text-red-800',
  Suspended: 'bg-yellow-100 text-yellow-800',
}

const priorityColors: Record<number, string> = {
  1: 'bg-gray-100 text-gray-800',
  2: 'bg-blue-100 text-blue-800',
  3: 'bg-green-100 text-green-800',
  4: 'bg-orange-100 text-orange-800',
  5: 'bg-red-100 text-red-800',
}

export default function WorkitemsPage() {
  const [workitems, setWorkitems] = useState<Workitem[]>(mockWorkitems)

  return (
    <div className="space-y-6">
      <div className="space-y-2">
        <h1 className="text-3xl font-bold">Workitems</h1>
        <p className="text-slate-600">Monitor tasks assigned to resources</p>
      </div>

      <div className="flex gap-2">
        <Button variant="outline">Filter</Button>
        <Button variant="outline">Refresh</Button>
      </div>

      <Card className="p-6">
        <div className="overflow-x-auto">
          <table className="w-full text-sm">
            <thead>
              <tr className="border-b">
                <th className="text-left py-3 px-4 font-semibold text-slate-700">Workitem ID</th>
                <th className="text-left py-3 px-4 font-semibold text-slate-700">Task</th>
                <th className="text-left py-3 px-4 font-semibold text-slate-700">Case</th>
                <th className="text-left py-3 px-4 font-semibold text-slate-700">Status</th>
                <th className="text-left py-3 px-4 font-semibold text-slate-700">Priority</th>
                <th className="text-left py-3 px-4 font-semibold text-slate-700">Assigned To</th>
                <th className="text-left py-3 px-4 font-semibold text-slate-700">Due Date</th>
                <th className="text-left py-3 px-4 font-semibold text-slate-700">Actions</th>
              </tr>
            </thead>
            <tbody>
              {workitems.map((wi) => (
                <tr key={wi.id} className="border-b hover:bg-slate-50">
                  <td className="py-3 px-4 font-mono text-xs">{wi.id}</td>
                  <td className="py-3 px-4">{wi.taskName}</td>
                  <td className="py-3 px-4 font-mono text-xs">{wi.caseId}</td>
                  <td className="py-3 px-4">
                    <Badge className={statusColors[wi.status]}>
                      {wi.status}
                    </Badge>
                  </td>
                  <td className="py-3 px-4">
                    {wi.priority && (
                      <Badge className={priorityColors[wi.priority]}>
                        P{wi.priority}
                      </Badge>
                    )}
                  </td>
                  <td className="py-3 px-4">{wi.allocatedTo || '-'}</td>
                  <td className="py-3 px-4 text-xs">
                    {wi.dueDate ? wi.dueDate.toLocaleDateString() : '-'}
                  </td>
                  <td className="py-3 px-4">
                    <Button variant="ghost" size="sm">View</Button>
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      </Card>

      <div className="text-sm text-slate-500">
        Showing {workitems.length} workitems. Data will load from SPARQL endpoint when configured.
      </div>
    </div>
  )
}

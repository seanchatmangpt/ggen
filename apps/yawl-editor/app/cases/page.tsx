'use client'

import { useState } from 'react'
import { Card } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import type { Case, CaseStatus } from '@/lib/types'

const mockCases: Case[] = [
  {
    id: 'case-001',
    status: 'Running' as CaseStatus,
    createdDate: new Date('2024-01-15'),
    completedDate: undefined,
    processId: 'proc-001',
    processName: 'Loan Application',
    ownerId: 'user-001',
    ownerName: 'John Smith',
  },
  {
    id: 'case-002',
    status: 'Completed' as CaseStatus,
    createdDate: new Date('2024-01-10'),
    completedDate: new Date('2024-01-12'),
    processId: 'proc-002',
    processName: 'Insurance Claim',
    ownerId: 'user-002',
    ownerName: 'Jane Doe',
  },
  {
    id: 'case-003',
    status: 'Suspended' as CaseStatus,
    createdDate: new Date('2024-01-18'),
    completedDate: undefined,
    processId: 'proc-001',
    processName: 'Loan Application',
    ownerId: 'user-001',
    ownerName: 'John Smith',
  },
]

const statusColors: Record<CaseStatus, string> = {
  Created: 'bg-gray-100 text-gray-800',
  Running: 'bg-blue-100 text-blue-800',
  Suspended: 'bg-yellow-100 text-yellow-800',
  Completed: 'bg-green-100 text-green-800',
  Cancelled: 'bg-red-100 text-red-800',
}

export default function CasesPage() {
  const [cases, setCases] = useState<Case[]>(mockCases)

  return (
    <div className="space-y-6">
      <div className="space-y-2">
        <h1 className="text-3xl font-bold">Cases</h1>
        <p className="text-slate-600">Manage workflow case instances</p>
      </div>

      <div className="flex gap-2">
        <Button>Create New Case</Button>
        <Button variant="outline">Refresh</Button>
      </div>

      <Card className="p-6">
        <div className="overflow-x-auto">
          <table className="w-full text-sm">
            <thead>
              <tr className="border-b">
                <th className="text-left py-3 px-4 font-semibold text-slate-700">Case ID</th>
                <th className="text-left py-3 px-4 font-semibold text-slate-700">Process</th>
                <th className="text-left py-3 px-4 font-semibold text-slate-700">Status</th>
                <th className="text-left py-3 px-4 font-semibold text-slate-700">Owner</th>
                <th className="text-left py-3 px-4 font-semibold text-slate-700">Created</th>
                <th className="text-left py-3 px-4 font-semibold text-slate-700">Actions</th>
              </tr>
            </thead>
            <tbody>
              {cases.map((c) => (
                <tr key={c.id} className="border-b hover:bg-slate-50">
                  <td className="py-3 px-4 font-mono text-xs">{c.id}</td>
                  <td className="py-3 px-4">{c.processName}</td>
                  <td className="py-3 px-4">
                    <Badge className={statusColors[c.status]}>
                      {c.status}
                    </Badge>
                  </td>
                  <td className="py-3 px-4">{c.ownerName || '-'}</td>
                  <td className="py-3 px-4 text-xs">
                    {c.createdDate.toLocaleDateString()}
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
        Showing {cases.length} cases. Data will load from SPARQL endpoint when configured.
      </div>
    </div>
  )
}

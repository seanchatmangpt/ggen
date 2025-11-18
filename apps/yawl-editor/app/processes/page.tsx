'use client'

import { useState } from 'react'
import { Card } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import type { Process } from '@/lib/types'

const mockProcesses: Process[] = [
  {
    id: 'proc-001',
    name: 'Loan Application',
    version: '1.0',
    description: 'Process for handling loan applications',
    createdDate: new Date('2024-01-01'),
    tasks: [
      { id: 'task-001', name: 'Review Application', processId: 'proc-001', description: 'Initial review' },
      { id: 'task-002', name: 'Process Documents', processId: 'proc-001', description: 'Document processing' },
      { id: 'task-003', name: 'Make Decision', processId: 'proc-001', description: 'Final decision' },
    ],
  },
  {
    id: 'proc-002',
    name: 'Insurance Claim',
    version: '2.1',
    description: 'Process for insurance claim handling',
    createdDate: new Date('2024-01-05'),
    tasks: [
      { id: 'task-004', name: 'Initial Assessment', processId: 'proc-002', description: 'Claim assessment' },
      { id: 'task-005', name: 'Investigation', processId: 'proc-002', description: 'Claim investigation' },
    ],
  },
  {
    id: 'proc-003',
    name: 'Customer Onboarding',
    version: '1.5',
    description: 'New customer onboarding process',
    createdDate: new Date('2024-01-10'),
    tasks: [
      { id: 'task-006', name: 'Verify Identity', processId: 'proc-003', description: 'Identity verification' },
      { id: 'task-007', name: 'Setup Account', processId: 'proc-003', description: 'Account setup' },
    ],
  },
]

export default function ProcessesPage() {
  const [processes, setProcesses] = useState<Process[]>(mockProcesses)
  const [selectedProcess, setSelectedProcess] = useState<Process | null>(null)

  return (
    <div className="space-y-6">
      <div className="space-y-2">
        <h1 className="text-3xl font-bold">Processes</h1>
        <p className="text-slate-600">Define and manage workflow processes</p>
      </div>

      <div className="flex gap-2">
        <Button>Create New Process</Button>
        <Button variant="outline">Refresh</Button>
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
        {/* Process List */}
        <div className="lg:col-span-2 space-y-4">
          {processes.map((proc) => (
            <Card
              key={proc.id}
              className={`p-6 cursor-pointer transition hover:shadow-md ${
                selectedProcess?.id === proc.id ? 'ring-2 ring-blue-500' : ''
              }`}
              onClick={() => setSelectedProcess(proc)}
            >
              <div className="space-y-3">
                <div className="flex items-start justify-between">
                  <div>
                    <h3 className="text-lg font-semibold">{proc.name}</h3>
                    <p className="text-sm text-slate-600">{proc.description}</p>
                  </div>
                  <span className="text-xs font-mono bg-slate-100 px-2 py-1 rounded">
                    v{proc.version}
                  </span>
                </div>
                <div className="flex gap-2">
                  <Button variant="ghost" size="sm">Edit</Button>
                  <Button variant="ghost" size="sm">Delete</Button>
                </div>
              </div>
            </Card>
          ))}
        </div>

        {/* Process Details */}
        <div>
          {selectedProcess ? (
            <Card className="p-6 sticky top-20">
              <div className="space-y-4">
                <div>
                  <h3 className="font-semibold text-slate-700">Process Details</h3>
                </div>
                <div className="space-y-3 text-sm">
                  <div>
                    <p className="text-slate-600">Name</p>
                    <p className="font-mono">{selectedProcess.name}</p>
                  </div>
                  <div>
                    <p className="text-slate-600">Version</p>
                    <p className="font-mono">{selectedProcess.version}</p>
                  </div>
                  <div>
                    <p className="text-slate-600">Created</p>
                    <p className="font-mono">
                      {selectedProcess.createdDate?.toLocaleDateString()}
                    </p>
                  </div>
                  <div>
                    <p className="text-slate-600 mb-2">Tasks ({selectedProcess.tasks?.length || 0})</p>
                    <div className="space-y-2">
                      {selectedProcess.tasks?.map((task) => (
                        <div key={task.id} className="bg-slate-50 p-2 rounded text-xs">
                          <p className="font-mono text-slate-700">{task.name}</p>
                        </div>
                      ))}
                    </div>
                  </div>
                </div>
              </div>
            </Card>
          ) : (
            <Card className="p-6">
              <p className="text-sm text-slate-500">Select a process to view details</p>
            </Card>
          )}
        </div>
      </div>

      <div className="text-sm text-slate-500">
        Showing {processes.length} processes. Data will load from SPARQL endpoint when configured.
      </div>
    </div>
  )
}

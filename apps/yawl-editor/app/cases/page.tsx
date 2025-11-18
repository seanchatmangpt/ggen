'use client'

import { useState, useCallback } from 'react'
import { Badge } from '@/components/ui/badge'
import { Button } from '@/components/ui/button'
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '@/components/ui/select'
import { Input } from '@/components/ui/input'
import { DataTable, Column } from '@/components/tables/data-table'
import { CreateCaseModal } from '@/components/modals/create-case-modal'
import { useSparqlSort, useSparqlSearch } from '@/hooks/use-sparql'
import { useModal } from '@/hooks/use-modal'
import { useAsync } from '@/hooks/use-async'
import { formatDate } from '@/lib/utils'
import type { Case, CaseStatus } from '@/lib/types'
import { SPARQL_QUERIES } from '@/lib/sparql-queries'

const statusColors: Record<CaseStatus, string> = {
  Created: 'bg-gray-100 text-gray-800',
  Running: 'bg-blue-100 text-blue-800',
  Suspended: 'bg-yellow-100 text-yellow-800',
  Completed: 'bg-green-100 text-green-800',
  Cancelled: 'bg-red-100 text-red-800',
}

const columns: Column<Case>[] = [
  { key: 'id', label: 'Case ID', sortable: true, width: '150px' },
  { key: 'processName', label: 'Process', sortable: true },
  {
    key: 'status',
    label: 'Status',
    sortable: true,
    render: (value) => (
      <Badge className={statusColors[value as CaseStatus]}>
        {value}
      </Badge>
    ),
  },
  { key: 'ownerName', label: 'Owner', sortable: true },
  {
    key: 'createdDate',
    label: 'Created',
    sortable: true,
    render: (value) => formatDate(value),
  },
]

export default function CasesPage() {
  const { isOpen, open, close } = useModal()
  const [statusFilter, setStatusFilter] = useState<string>('')

  // Fetch cases from API
  const { data: allCases, execute: refetch, loading, error } = useAsync(
    async () => {
      const response = await fetch(
        `/api/cases${statusFilter ? `?status=${statusFilter}` : ''}`
      )
      if (!response.ok) throw new Error('Failed to fetch cases')
      const result = await response.json()
      return result.data as Case[]
    },
    true
  )

  // Advanced sorting
  const {
    data: sortedData,
    sortField,
    sortOrder,
    toggleSort,
  } = useSparqlSort(
    SPARQL_QUERIES.getAllCases,
    'createdDate',
    'desc'
  )

  // Search functionality
  const [searchTerm, setSearchTerm] = useState('')
  const filteredData = searchTerm
    ? allCases.filter((c) =>
        [c.id, c.processName, c.ownerName].some((field) =>
          field?.toLowerCase().includes(searchTerm.toLowerCase())
        )
      )
    : allCases

  const handleRefresh = useCallback(async () => {
    await refetch()
  }, [refetch])

  const handleStatusFilter = (status: string) => {
    setStatusFilter(status === 'all' ? '' : status)
  }

  const handleSuccess = () => {
    handleRefresh()
  }

  return (
    <div className="space-y-6">
      <div className="space-y-2">
        <h1 className="text-3xl font-bold">Cases</h1>
        <p className="text-slate-600">Manage workflow case instances</p>
      </div>

      {/* Toolbar */}
      <div className="flex flex-col sm:flex-row gap-3 items-start sm:items-center justify-between">
        <div className="flex gap-2">
          <Button onClick={open}>Create New Case</Button>
          <Button variant="outline" onClick={handleRefresh}>
            Refresh
          </Button>
        </div>

        <div className="flex gap-2 w-full sm:w-auto">
          <Input
            placeholder="Search cases..."
            value={searchTerm}
            onChange={(e) => setSearchTerm(e.target.value)}
            className="flex-1 sm:flex-none"
          />

          <Select value={statusFilter || 'all'} onValueChange={handleStatusFilter}>
            <SelectTrigger className="w-[150px]">
              <SelectValue placeholder="Filter by status" />
            </SelectTrigger>
            <SelectContent>
              <SelectItem value="all">All Status</SelectItem>
              <SelectItem value="Created">Created</SelectItem>
              <SelectItem value="Running">Running</SelectItem>
              <SelectItem value="Suspended">Suspended</SelectItem>
              <SelectItem value="Completed">Completed</SelectItem>
              <SelectItem value="Cancelled">Cancelled</SelectItem>
            </SelectContent>
          </Select>
        </div>
      </div>

      {/* Data Table */}
      <DataTable
        columns={columns}
        data={filteredData}
        loading={loading}
        error={error}
        sortField={sortField}
        sortOrder={sortOrder}
        onSort={toggleSort}
        getRowKey={(row) => row.id}
      />

      {/* Summary */}
      <div className="flex justify-between text-sm text-slate-600">
        <span>Showing {filteredData.length} cases</span>
        {searchTerm && <span>Search results for "{searchTerm}"</span>}
      </div>

      {/* Modals */}
      <CreateCaseModal
        isOpen={isOpen}
        onClose={close}
        onSuccess={handleSuccess}
      />
    </div>
  )
}

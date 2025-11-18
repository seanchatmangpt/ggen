'use client'

import {
  ChevronUp,
  ChevronDown,
  ChevronsUpDown,
  Loader2,
  AlertCircle,
} from 'lucide-react'
import { Button } from '@/components/ui/button'
import { Card } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'

export interface Column<T> {
  key: keyof T
  label: string
  sortable?: boolean
  render?: (value: any, row: T) => React.ReactNode
  width?: string
}

interface DataTableProps<T> {
  columns: Column<T>[]
  data: T[]
  loading?: boolean
  error?: Error | null
  sortField?: keyof T
  sortOrder?: 'asc' | 'desc'
  onSort?: (field: keyof T) => void
  onRowClick?: (row: T) => void
  getRowKey: (row: T) => string
}

export function DataTable<T extends Record<string, any>>({
  columns,
  data,
  loading = false,
  error = null,
  sortField,
  sortOrder = 'asc',
  onSort,
  onRowClick,
  getRowKey,
}: DataTableProps<T>) {
  const getSortIcon = (key: keyof T) => {
    if (sortField !== key) {
      return <ChevronsUpDown className="h-4 w-4 text-slate-400" />
    }
    return sortOrder === 'asc' ? (
      <ChevronUp className="h-4 w-4" />
    ) : (
      <ChevronDown className="h-4 w-4" />
    )
  }

  if (error) {
    return (
      <Card className="p-8">
        <div className="flex gap-3 text-destructive">
          <AlertCircle className="h-5 w-5 flex-shrink-0 mt-0.5" />
          <div>
            <h3 className="font-semibold">Error Loading Data</h3>
            <p className="text-sm">{error.message}</p>
          </div>
        </div>
      </Card>
    )
  }

  if (loading && data.length === 0) {
    return (
      <Card className="p-8">
        <div className="flex gap-3 items-center justify-center text-slate-600">
          <Loader2 className="h-5 w-5 animate-spin" />
          <span>Loading data...</span>
        </div>
      </Card>
    )
  }

  if (data.length === 0) {
    return (
      <Card className="p-8">
        <div className="text-center text-slate-600">
          <p className="text-sm">No data available</p>
        </div>
      </Card>
    )
  }

  return (
    <Card className="overflow-hidden">
      <div className="overflow-x-auto">
        <table className="w-full text-sm">
          <thead className="bg-slate-50 border-b">
            <tr>
              {columns.map((column) => (
                <th
                  key={String(column.key)}
                  className={`text-left py-3 px-4 font-semibold text-slate-700 ${
                    column.width ? `w-[${column.width}]` : ''
                  }`}
                >
                  {column.sortable && onSort ? (
                    <Button
                      variant="ghost"
                      size="sm"
                      className="h-8 gap-2 -ml-2"
                      onClick={() => onSort(column.key)}
                    >
                      {column.label}
                      {getSortIcon(column.key)}
                    </Button>
                  ) : (
                    column.label
                  )}
                </th>
              ))}
            </tr>
          </thead>
          <tbody>
            {data.map((row) => (
              <tr
                key={getRowKey(row)}
                className={`border-b hover:bg-slate-50 transition ${
                  onRowClick ? 'cursor-pointer' : ''
                }`}
                onClick={() => onRowClick?.(row)}
              >
                {columns.map((column) => (
                  <td
                    key={String(column.key)}
                    className="py-3 px-4"
                  >
                    {column.render ? (
                      column.render(row[column.key], row)
                    ) : typeof row[column.key] === 'boolean' ? (
                      <Badge variant={row[column.key] ? 'default' : 'secondary'}>
                        {String(row[column.key])}
                      </Badge>
                    ) : (
                      String(row[column.key] ?? '-')
                    )}
                  </td>
                ))}
              </tr>
            ))}
          </tbody>
        </table>
      </div>
      {loading && data.length > 0 && (
        <div className="flex gap-2 items-center p-4 border-t bg-slate-50 text-sm text-slate-600">
          <Loader2 className="h-4 w-4 animate-spin" />
          <span>Loading more...</span>
        </div>
      )}
    </Card>
  )
}
